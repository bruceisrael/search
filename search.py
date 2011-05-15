#!/usr/bin/env python
#
# search - grep on steroids
#     Written by: Bruce Israel <israel@tux.org>, Mon Jan 23 2006
#
#    **** See end of file for documentation ****

import sys
import os
import stat
import re
import gzip
import tarfile
import zipfile
import signal

def ifElse(a,b,c): return (b,c)[not a]

def errorMessage(state, errortype, msg):
    '''
    Handle error messages, deciding whether to print the message or not.

    Takes different characterizations of the type of error:
    ABORT - can't proceed; show the error and exit
    USER - display the error message and continue
    anything else (software errors) - only show the message if the ShowErrors
      option (-se) was specified.
    '''
    (show, abort) = (False, False)
    if errortype == "ABORT":
        (show, abort) = (True, True)
    elif errortype == "USER":
        show = True
    elif state and state.getState("ShowErrors"):
        show = True
    if show: print >> sys.stderr, msg
    if abort: sys.exit(1)

def usage(prog, msg = None):
    # TODO: see if this list can be auto-generated from options definitions
    args = ["[ -f ] <file or dir>",
            "[ -e ] <search-expr>",
            "-fe <filename-expr>",
            "-start <block-start-expr>",
            "-end <block-end-expr>",
            "-whole",
            "-v", "+v",
            "-i", "+i",
            "-s", "+s",
            "-E", "+E",
            "-F", "+F",
            "-se",
            "-d[cflbn]"]
    if msg:
        print >> sys.stderr, msg
    if prog:
        print >> sys.stderr, "Usage: %s [ %s ] ..." % (prog, " | ".join(args))
    sys.exit(1)

#
# SearchStrategy implements a strategy pattern that controls what
# blocks of the content source are to be searched.  Each
# implementation needs an __init__ method to set the state and a
# getBlock() method that works as a generator, returning a text
# block along with a count each time it is called.
#
# Currently three strategies are implemented:
# Lines - search for lines that match the search strings
# Blocks - search for blocks of lines that match the search strings
# Whole File - check the entire file for the search strings
#
class SearchStrategy:
    def __init__(self, state):
        self.state = state
    def getBlock(self, file):
        pass # Override me in derived class
    def doSearch(self, contents, name):
        try:
            for (block, linenum) in self.getBlock(contents):
                if self.state.SearchExprs.areAllSatisfiedBy(block):
                    disphandler = self.state.getState("DisplayHandler")
                    if not disphandler(name, linenum, block):
                        return
        except TypeError:
            # TypeError is caught here because sometimes with files opened from
            # things like tarfiles; e.g. tar.extractfile(tarmember), it doesn't
            # work as a sequence generator and reports that it's attempting to
            # loop on a non-sequence object
            errorMessage(self.state, "SOFTWARE", "Type error on file %s" % name)

class SearchStrategyLines(SearchStrategy):
    def __init__(self, state):
        SearchStrategy.__init__(self, state)
    def getBlock(self, contents):
        '''A generator that returns each line of the content'''
        linenum = 0
        for line in contents:
            linenum += 1
            yield (line, linenum)

class SearchStrategyBlocks(SearchStrategy):
    def __init__(self, state):
        SearchStrategy.__init__(self, state)
        self.initialSearch = self.makeSearches()
    def getBlock(self, file):
        '''A generator that returns blocks of the file delimited by lines
        matching the blockStart list and lines matching the blockEnd list.

        Specifying START means that returned blocks will begin with
        START.  Specifying END means that returned blocks will end with
        END.  If both are present, then only groups of lines that
        ranging from START to END will be returned, and all other lines
        discarded.  If only START is specified, then all lines from the
        beginning of the file thru (but not including) START will be
        discarded, and all blocks from START up through (but not
        including) the next START will be returned, and the final block
        would be the last START line through the END of the file.  If
        only END is specified, then the first block will be the
        beginning of the file thru (and including) END, and all
        subsequent blocks will be from after the previous END through
        the next END.  Any lines following the last END in the file will
        be discarded.'''
        curSearch = self.initialSearch
        curBlock = ""
        linenum = 0
        blockstartNum = linenum
        for line in file:
            linenum += 1
            if not curSearch.searchMatches(line):
                curBlock = curSearch.update(curBlock, line)
            else:
                curBlock = curSearch.updateBefore(curBlock, line)
                if curSearch.useLines:
                    yield (curBlock, blockstartNum)
                    blockstartNum = linenum
                curBlock = curSearch.updateAfter("", line)
                curSearch = curSearch.next
        if curSearch.useLinesAtEOF and curBlock:
            yield (curBlock, blockstartNum)
    def makeSearches(self):
        state = self.state
        blockStart = state.blockStart
        blockEnd = state.blockEnd
        # search patterns
        searchForStartDiscardingTilFound = SearchElement(False, False, False, blockStart)
        searchForEndDiscardingAfterwards = SearchElement(True, False, True, blockEnd)
        searchForStartKeeping = SearchElement(True, True, False, blockStart)
        if blockStart and blockEnd:
            # If we have both START and END:
            #  Start looking for START, ignoring everything till we see it
            #  Once we've seen it, then look for END, keeping that data
            #  And after that, ignore everything till the next START
            #  and ignore everything left over when we've hit the end.
            startingSearch = searchForStartDiscardingTilFound
            startingSearch.next = searchForEndDiscardingAfterwards
            startingSearch.next.next = startingSearch
        elif blockStart:
            # If we have only START:
            #  Start looking for START,
            #  ignore everything till we see it,
            #  Once we've seen it, then look again for START, keeping that data
            #  and keep everything left over when we've hit the end.
            startingSearch = searchForStartDiscardingTilFound
            startingSearch.next = searchForStartKeeping
            startingSearch.next.next = startingSearch.next
        elif blockEnd:
            # If we have only END:
            #  Start looking for END,
            #  keep everything till we see it,
            #  Once we've seen it, then look again for END, keeping that data
            #  but ignore everything left over when we've hit the end.
            startingSearch = searchForEndDiscardingAfterwards
            startingSearch.next = startingSearch
        else:
            # Should never get here since this is only called for
            # line-oriented block searches
            return
        return startingSearch


class SearchStrategyWholeFile(SearchStrategy):
    def __init__(self, state):
        SearchStrategy.__init__(self, state)
    def getBlock(self, contents):
        '''A generator that returns the entire file contents'''
        # One problem with this approach is that it requires whole
        # file searches to load in the entire file contents at once,
        # making this problematic for use where the file is very
        # large; the approach probably should be reworked if
        # possible to allow for searching of the content while it's
        # being streamed.
        yield ("".join(contents), 1)


class SearchElement:
    '''
    This class is only used for line-oriented block searches to
    determine what lines to look for and what to do with the lines
    seen.
    '''
    def __init__(self, useLines, useLinesAtEOF, endsBlock, exprs):
        self.useLines = useLines
        self.useLinesAtEOF = useLinesAtEOF
        self.endsBlock = endsBlock
        self.exprs = exprs
    def searchMatches(self, line):
        # TODO: determine if multiplicity is needed here
        for expr in self.exprs:
            if not expr.exprMatches(line): return False
        return True
    def update(self, start, line):
        return start + line
    def updateBefore(self, start, line):
        if self.endsBlock:
            return start + line
        else:
            return start
    def updateAfter(self, start, line):
        if self.endsBlock:
            return start
        else:
            return start + line


# PartitionedExprs represent collections of expressions, partitioned
# into those can cause the match to fail (the skipList), and those
# that are looking to be satisfied, either all satisfied or any
# satisfied (the matchList).  Partitioning is done based on the
# 'invertMatch' flag that each expression has.
class PartitionedExprs:
    def __init__(self, exprs):
        self.matchList = []
        self.skipList = []
        for expr in exprs:
            if expr.invertMatch:
                self.skipList.append(expr)
            else:
                self.matchList.append(expr)
    def shouldSkip(self, searchstr):
        # If any items in the skip list match, then criteria don't match
        return self.doAnyMatch(searchstr, self.skipList, True)
    def areAllSatisfiedBy(self, searchstr):
        if self.shouldSkip(searchstr):
            return False
        # If we have nothing in the positive match list, then we are
        # done (successfully)
        if len(self.matchList) == 0:
            return True
        # If any expressions in the positive match list fail, then we don't match
        return not self.doAnyMatch(searchstr, self.matchList, False)
    def areAnySatisfiedBy(self, searchstr):
        if self.shouldSkip(searchstr):
            return False
        # If we have nothing in the positive match list, then we are
        # done (successfully)
        if len(self.matchList) == 0:
            return True
        # If any expressions in the positive match list match, then we match
        return self.doAnyMatch(searchstr, self.matchList, True)
    def doAnyMatch(self, searchstr, exprs, expectedValue):
        '''See if any expressions in the expressions list evaluate to
        the expected value.'''
        for expr in exprs:
            if expr.exprMatches(searchstr) == expectedValue:
                return True
        return False


class GlobalState:
    def __init__(self, prog):
        self.prog = prog
        # All settings default to false unless otherwise specified
        self.SettingsMap = {}
        self.FileList = []
        self.SearchExprList = []
        self.FilenameExprList = []
        self.blockStart = []
        self.blockEnd = []
        # Set up the different types of files that can be read,
        # along with the semantic handlers that know how to process
        # them.
        #
        # Note: the order of these readers is important and
        # specifies the precedence.  Because of a bug in the Python
        # gzip module, the tarfile module cannot read a tarfile
        # streamed from within a gzip file, so TarFileReader must
        # precede GzipFileReader here and uses its own gzip decoding.
        self.fileReaders = [
            LinkFileReader(self),
            UnreadableFileReader(self),
            DirFileReader(self),
            TarFileReader(self),
            ZipFileReader(self),
            CompressFileReader(self),
            GzipFileReader(self),
            RegularFileReader(self)
            ]
    def blockBased(self):
        return self.blockStart or self.blockEnd or self.getState("WholeFile")
    def getState(self, key):
        if key not in self.SettingsMap: return False
        return self.SettingsMap[key]
    def finalizeState(self):
        '''Finalize state settings.

        Set any default values needed, partition the expressions
        lists (both the search and the file expressions) into match
        and skip lists, and select the proper search strategy.'''
        # if no files specified, then default to STDIN
        if len(self.FileList) == 0:
            self.FileList.append(StdinHandler())
        # If no display handler was explicitly specified,
        # then the default should be line-based ('-dl' option) for
        # normal searches and block-based ('-db' option) when doing
        # a multi-line search.
        if not "DisplayHandler" in self.SettingsMap:
            self.SettingsMap["DisplayHandler"] = (
                lambda name, numIgnored, line:
                  doDisplay(name, None, self.blockBased(), line)
        )
        # Partition Search Expressions list
        self.SearchExprs = PartitionedExprs(self.SearchExprList)
        # Partition Filename Expressions list
        self.FilenameExprs = PartitionedExprs(self.FilenameExprList)
        # Set search strategy
        if self.getState("WholeFile"):
            self.searchStrategy = SearchStrategyWholeFile(self)
        elif self.blockBased():
            self.searchStrategy = SearchStrategyBlocks(self)
        else:
            self.searchStrategy = SearchStrategyLines(self)

def signal_handler(signalIgnored, frameIgnored):
    print >> sys.stderr, 'User interrupt.'
    sys.exit(0)

def main(args):
    signal.signal(signal.SIGINT, signal_handler)
    prog = args.pop(0)
    if len(args) == 0: usage(prog)
    # Set up initial state and options definitions
    state = GlobalState(prog)
    opts = defineOpts(state)
    # parse args
    while len(args) > 0:
        matchAndStoreOpt(args, opts)
    # Do consistency checks
    if "ShowHelp" in state.SettingsMap:
        usage(prog, getHelpString(state.SettingsMap["ShowHelp"]))
    # error if no search expressions
    if len(state.SearchExprList) == 0:
        usage(prog, "No search terms specified.")
    # error if no files and there are filename expressions
    if len(state.FileList) == 0 and len(state.FilenameExprList) > 0:
        usage(prog, "Expecting search files when given filename "
              "expressions (e.g. %s)." % state.FilenameExprList[0])
    # Finalize state settings
    state.finalizeState()
    # run all files
    processFiles(state.FileList, state)

def processFiles(fileList, state):
    for fileref in fileList:
        # A filename passes the FilenameExprs filters if
        # 1) NO skip expressions match, and
        # 2) at least one match expression matches if any are present
        if (fileref.skipNameCheck() or
            state.FilenameExprs.areAnySatisfiedBy(fileref.getName())):
            fileref.searchFile(state)

################################################################
# Options Parsing

def doDisplay(fname, linenum, headerSep, line):
    '''Function to generate match output.

    Parameters supply what needs to be displayed, and can be None if
    a particular piece of information should not be displayed. '''
    matchIdFields = []
    if fname: matchIdFields.append(fname)
    if linenum: matchIdFields.append(str(linenum))
    if headerSep:
        matchIdFields.append("\n")
    elif line:
        matchIdFields.append("")
    outputString = ":".join(matchIdFields)
    if line: outputString = "%s%s" % (outputString, line.rstrip())
    print outputString
    return line

def defineOpts(state):
    '''Define the options than this script accepts and returns a list.

    Each entry in the list is an Option entity that has methods that
    determine if it matches an parameter string, and if so, how to
    handle it.

    The order of this list is important and determines precedence;
    the first Option that can handle a particular parameter will
    take effect.  The last entry needs to be a catch-all that always
    matches anything not previously caught.

    Options that take additional information can consume parameters
    for use and which will not get further parsed.
    '''
    opts=[]
    # -f - following arg is a filesytem object to be searched
    opts.append(Opt("-f", state, True, SearchableFileHandler, state.FileList))
    # -e - following arg is a search expression
    opts.append(Opt("-e", state, True, ExprHandler, state.SearchExprList))
    # -fe - following arg is a filename expression to match
    opts.append(Opt("-fe", state, True, ExprHandler, state.FilenameExprList))
    # -help|--help - displays program help
    opts.append(SettingsOpt(state, "ShowHelp",
                            matching = "-help", value = 1))
    opts.append(SettingsOpt(state, "ShowHelp",
                            matching = "--help", value = 2))
    # -v|+v - turn on/off inverting of the match (like 'grep -v')
    opts.append(BooleanSettingsOpt(state, "InvertMatch",
                                   enable = "-v", disable = "+v"))
    # -i|+i - turn on/off case-insensitive matching (like 'grep -i')
    opts.append(BooleanSettingsOpt(state, "IgnoreCase",
                                   enable = "-i", disable = "+i"))
    # -s|+s - turn on/off treating subsequent matching expressions as 
    #   search strings (vs as regular expressions)
    opts.append(BooleanSettingsOpt(state, "TreatExprsAsStrings",
                                   enable = "-s", disable = "+s"))
    # -E|+E - Treat subsequent parameters as only matching
    #   expressions (short-circuiting process that determines whether
    #   it's a file or an expression) or not
    opts.append(BooleanSettingsOpt(state, "ExprOnly",
                                   enable = "-E", disable = "+E"))
    # -F|+F - Treat subsequent parameters as only filesytem objects or not 
    opts.append(BooleanSettingsOpt(state, "FilesOnly",
                                   enable = "-F", disable = "+F"))
    # -se option is used to display software errors also
    opts.append(SettingsOpt(state, "ShowErrors",
                            matching = "-se", value = True))
    # dc - display only matching file contents
    opts.append(SettingsOpt(state, "DisplayHandler",
                            matching = "-dc", 
                            value = lambda name, num, line: (
                              doDisplay(None, None, False, line))))
    # df - display just filenames
    opts.append(SettingsOpt(state, "DisplayHandler",
                            matching = "-df", 
                            value = lambda name, num, line: (
                              doDisplay(name, None, False, None))))
    # dl - display filename with matching lines
    opts.append(SettingsOpt(state, "DisplayHandler",
                            matching = "-dl", 
                            value = lambda name, num, line: (
                              doDisplay(name, None, False, line))))
    # dn - display filename with line numbers and matching lines
    opts.append(SettingsOpt(state, "DisplayHandler",
                            matching = "-dn",
                            value = lambda name, num, line: (
                              doDisplay(name, num, False, line))))
    # db - display blocks of lines with filename and line number on a separate
    # preceding line
    opts.append(SettingsOpt(state, "DisplayHandler",
                            matching = "-db", 
                            value = lambda name, num, line: (
                              doDisplay(name, num, True, line))))
    # indicate delimeters for block searching
    opts.append(Opt("-start", state, True, ExprHandler, state.blockStart))
    opts.append(Opt("-end", state, True, ExprHandler, state.blockEnd))
    opts.append(SettingsOpt(state, "WholeFile",
                            matching = "-whole", value = True))
    # default options for everything else; If the parameter matches
    # the name of a file, then treat it as a file to be searched and
    # treat anything else as an expression to be searched for.
    opts.append(FileDefaultOpt(SearchableFileHandler, state.FileList, state))
    opts.append(DefaultOpt(ExprHandler, state.SearchExprList, state))
    return opts

def matchAndStoreOpt(args, opts):
    '''Process the option list to find the first applicable option for each parameter.'''
    opt = args.pop(0)
    for poss in opts:
        if poss.match(opt):
            poss.handle(opt, args)
            return
    # Should never get here
    print >> sys.stderr, "No handler found for %s" % opt
    sys.exit(1)

class Opt:
    def __init__(self, optstr, state, consume, handler, varlist):
        self.optstr = optstr
        self.state = state
        self.consume = consume
        self.handler = handler
        self.varlist = varlist
    def match(self, str):
        return str == self.optstr
    def handle(self, param, args):
        if self.consume: param = args.pop(0)
        self.varlist.append(self.handler(param, self.state))

class SettingsOpt:
    def __init__(self, state, key, matching, value):
        self.state = state
        self.key = key
        self.matching = matching
        self.value = value
    def match(self, param):
        return param == self.matching
    def handle(self, paramIgnored, argsIgnored):
        self.state.SettingsMap[self.key] = self.value

class BooleanSettingsOpt:
    def __init__(self, state, key, enable, disable):
        self.state = state
        self.key = key
        self.enable = enable
        self.disable = disable
        self.map = state.SettingsMap
    def match(self, param):
        return param == self.enable or param == self.disable
    def handle(self, param, argsIgnored):
        self.map[self.key] = (param == self.enable)

class DefaultOpt(Opt):
    def __init__(self, handler, varlist, state):
        Opt.__init__(self, "", state, False, handler, varlist)
    def match(self, param):
        return True

class FileDefaultOpt(DefaultOpt):
    def match(self, fname):
        if self.state.getState("ExprOnly"): return False
        if os.path.exists(fname): return True
        if self.state.getState("FilesOnly"):
            errorMessage(self.state, "USER", "Non-existent file: %s" % fname)
            return True
        return False

################################################################
# Semantic Items Handlers


class StdinHandler:
    def getName(self): return None
    def skipNameCheck(self):
        return True
    def searchFile(self, state):
        state.searchStrategy.doSearch(sys.stdin, None)


class ExprHandler:
    def __init__(self, searchExpr, state):
        self.searchExpr = searchExpr
        self.state = state
        # instantiate state AT THE TIME OF CONSTRUCTION
        #   of state variables invertMatch and ignoreCase
        # invertMatch is used to partition expression list into skip and 
        #   match lists; any expressions on the skip list force a match failure
        self.invertMatch = state.getState("InvertMatch")
        # ignoreCase is used to control match processing
        self.ignoreCase = state.getState("IgnoreCase")
        # TreatExprsAsStrings is used to pass strings for matching
        self.treatExprsAsStrings = state.getState("TreatExprsAsStrings")        
        if self.ignoreCase:
            self.searchExpr = self.searchExpr.lower()
        if not self.treatExprsAsStrings:
            try:
                self.compiledExpr = re.compile(self.searchExpr)
            except Exception, e:
                errorMessage(self.state, "ABORT",
                             "Error: Invalid expression '%s': %s" %
                             (self.searchExpr, e))
    def __str__(self):
        return "string%s matching %s" % (
            ifElse(self.invertMatch, " not", ""), self.searchExpr)
    def exprMatches(self, checkstr):
        '''Check if the string matches this expression.'''
        # Take into account the ignoreCase flag by lowercasing both the
        # expression (in the initializer) and the target string (here)
        if self.ignoreCase:
            checkstr = checkstr.lower()
        # This method is set up to return a True/False value, so the
        # following ifElse calls ensure that.  This way the return
        # value can used in a comparison.
        if self.treatExprsAsStrings:
            return ifElse(checkstr.find(self.searchExpr) >= 0, True, False)
        else:
            return ifElse(re.search(self.compiledExpr, checkstr), True, False)

class SearchableFileHandler:
    def __init__(self, fname, state, openFile = None):
        if fname == "-":
            fname = "<STDIN>"
            openFile = sys.stdin
        self.fname = fname
        self.state = state
        self.openFile = openFile
        self.reader = self.findReader(state)
        if not self.reader:
            # This should never happen
            errorMessage(self.state, "ABORT",
                         "No reader found for %s" % self.fname)
    def getName(self): return self.fname
    def __str__(self):
        return "%sfile named %s" % (ifElse(self.openFile, "opened ", ""),
                                    self.getName())
    def findReader(self, state):
        for reader in state.fileReaders:
            if reader.appliesTo(self): return reader
    def matchesOneOf(self, exts):
        fname = self.fname.lower()
        for ext in exts:
            if fname.endswith(ext): return True
        return False
    def skipNameCheck(self):
        # Directories (actually, any collections such as tar and zip
        # files too) should not be tested for required names since a
        # child may match the required names
        if self.reader.isCollection(self):
            return True
        if not self.openFile and not os.path.isfile(self.fname):
            errorMessage(self.state, "USER", "Non-existent file: %s" % self.fname)
        return False
    def searchFile(self, state):
        if self.reader.isCollection(self):
            processFiles(self.reader.getMembers(self), state)
        else:
            state.searchStrategy.doSearch(self.reader.getContents(self), self.getName())


class LinkFileReader:
    """
    a search reader to handle symbolic links.

    Symlinks are silently ignored (pending any changes to allow them to be
    traversed).
    """
    # TODO: add -fl,+fl (follow links) option to allow the user to
    # specify how to handle symlinks.
    def __init__(self, state):
        self.state = state
    def appliesTo(self, fileref):
        fname = fileref.getName()
        return os.path.islink(fname)
    def isCollection(self, fileref): return True
    def getMembers(self, filerefIgnored): return []

class UnreadableFileReader:
    """
    a search reader to handle non-existent or unreadable files.

    Unreadable files will print out an access error and then continue onto the
    next file.
    """
    def __init__(self, state):
        self.state = state
    def appliesTo(self, fileref):
        if fileref.openFile: return False
        if os.path.isdir(fileref.getName()): return False
        fname = fileref.getName()
        if not os.path.exists(fname): return True
        try:
            openfile = open(fname)
        except IOError, ex:
            errorMessage(self.state, "USER", "Cannot open %s: %s" % (fname, ex))
            return True
        openfile.close()
        return False
    def isCollection(self, filerefIgnored): return True
    def getMembers(self, filerefIgnored): return []

def inode(fname):
    return os.stat(fname)[stat.ST_INO]


class DirFileReader:
    """
    a search reader to handle directories on the file system.

    If already open, then it's not a directory.
    A directory is a collection of its members (name made to full pathnames).
    """
    def __init__(self, state):
        self.state = state
        self.dirsChecked=[]
    def appliesTo(self, fileref):
        if fileref.openFile: return False
        return os.path.isdir(fileref.getName())
    def isCollection(self, fileref):
        return True
    def getMembers(self, fileref):
        fname = fileref.getName()
        fileInode = inode(fname)
        if fileInode in self.dirsChecked:
            # Already seen, skipping
            return []
        self.dirsChecked.append(fileInode)
        try:
            members = sorted(os.listdir(fname))
        except Exception:
            errorMessage(self.state, "USER",
                  "Cannot read directory %s" % fname)
            return []
        fullnames = map(lambda file:
                          SearchableFileHandler(
                            os.path.normpath(fname + "/" + file), self.state),
                        members)
        return fullnames

# Create a pseudo filename for a file extracted from within another file;
# e.g. extracting file 'bar' from the tarfile 'foo.tar'.
# Since there isn't an accepted convention for this relationship,
# and using a single slash creates the ambiguity that the containing
# file may actually be a directory, we are choosing to use a
# double-slash as the notation to communicate this relationship;
# e.g. foo.tar//bar
def createContainedFileName(outer, inner):
    '''
    Create a pseudo filename for a file extracted from within another file.
    '''
    return "%s//%s" % (outer, inner)


class GzipFileReader:
    """
    a search reader to handle gzipped files.

    Applicable only for files matching '*.gz'.
    This will be a collection of one item.  This single item is the ungzipped
    contents of the gzip file, allowing that to be processed itself (e.g. if
    the ungzipped file is itself another file such as a tar file).

    """
    def __init__(self, state):
        self.state = state
        self.ext = ".gz"
    def appliesTo(self, fileref):
        return fileref.matchesOneOf([self.ext])
    def isCollection(self, fileref):
        return True
    def getMembers(self, fileref):
        fname = fileref.getName()
        try:
            if fileref.openFile:
                openFile = gzip.GzipFile(None, "r", 9, fileref.openFile)
            else:
                openFile = gzip.open(fileref.getName())
        except Exception, ex:
            errorMessage(self.state, "USER",
                         "Couldn't access zip file %s: %s" % (fileref, ex))
            return []
        extractedNameStart = fname.rfind("/") + 1
        extractedName = fname[extractedNameStart:- len(self.ext)]
        contentsName = createContainedFileName(fname, extractedName)
        searchFile = SearchableFileHandler(contentsName, self.state, openFile)
        return [searchFile]


class ZipFileReader:
    """
    a search reader to handle zipped files.

    Applicable only for files matching '*.zip'.

    This will be a collection of the files within the zip file.
    """
    def __init__(self, state):
        self.state = state
        self.ext = ".zip"
    def appliesTo(self, fileref):
        return fileref.matchesOneOf([self.ext])
    def isCollection(self, fileref):
        return True
    def getMembers(self, fileref):
        fname = fileref.getName()
        if fileref.openFile:
            try:
                zipcontainer = zipfile.ZipFile(fileref.openFile, "r")
            except Exception, e:
                errorMessage(self.state, "SOFTWARE",
                             "Can't open zipfile %s (from open file)"
                             " to get members: %s" % (fname, e))
                return
        else:
            zipcontainer = zipfile.ZipFile(fname, "r")
        for member in sorted(zipcontainer.namelist()):
            if not member.endswith("/"):
                name = createContainedFileName(fname, member)
                yield SearchableFileHandler(name, self.state,
                                            zipcontainer.open(member, "r"))
        zipcontainer.close()
    def getContents(self, fileref):
        return None


class CompressFileReader:
    """
    a search reader to handle compressed files.

    Applicable only for files matching '*.Z'.

    Not yet implemented.
    """
    # TODO: Implement this class
    def __init__(self, state):
        self.state = state
        self.ext = ".Z"
    def appliesTo(self, fileref):
        return False
    def isCollection(self, fileref):
        return True
    def getMembers(self, fileref):
        return []
    def getContents(self, fileref):
        return None


class TarFileReader:
    """
    a search reader to handle tar files.

    Applicable only for files matching '*.tar', '*.tar.gz' or '*.tgz'.

    This will be a collection of the files within the tar file.
    """
    def __init__(self, state):
        self.state = state
        self.exts = [".tar", ".tgz", ".tar.gz"]
    def appliesTo(self, fileref):
        return fileref.matchesOneOf(self.exts)
    def isCollection(self, fileref):
        return True
    def getMembers(self, fileref):
        mode = "r"
        fname = fileref.getName()
        if fileref.openFile:
            try:
                tar = tarfile.open(None, mode, fileref.openFile)
            except Exception, e:
                errorMessage(self.state, "SOFTWARE",
                             "Can't open tarfile %s (from open file)"
                             " to get members: %s" % (fname, e))
                return
        else:
            try:
                tar = tarfile.open(fname, mode)
            except Exception, ex:
                errorMessage(self.state, "USER",
                             "Couldn't open file %s: %s" % (fname, ex))
                return
        for member in tar.getmembers():
            if member.isfile():
                name = createContainedFileName(fname, member.name)
                yield SearchableFileHandler(name, self.state,
                                            tar.extractfile(member))
        tar.close()
    def getContents(self, fileref):
        return None


class RegularFileReader:
    def __init__(self, state):
        self.state = state
    def appliesTo(self, fileref):
        return True
    def isCollection(self, fileref):
        return False
    def getContents(self, fileref):
        if fileref.openFile:
            openfile = fileref.openFile
        else:
            openfile = open(fileref.getName())
        for line in openfile.readlines():
            yield line
        openfile.close()


#################################################################
# File comments/doc

def getHelpString(level):
    helpstr=''
    (showDoc, showExamples) = (False, False)
    if level > 0:
        showDoc = True
    if level > 1:
        showExamples = True
    if showDoc:
        helpstr += '''
search - search mutiple files for multiple patterns

Usage:
 search <args> ...

search takes a number of patterns and a number of files and does an
intersection search for the patterns in the files.  Patterns and
files can be intermixed, and the determination as to whether a
specified argument is a pattern or a file is whether it exists in
the filesystem; i.e. if it exists as a file, then it is treated as a
file, otherwise it's treated as a pattern, so the following two
commands are equivalent:

    search.py israel /etc/passwd bash
and
    search.py /etc/passwd israel bash 

and will both search for /etc/passwd entries containing the strings
israel and bash.

The determination of a string as an search expression or file can be
overridden by preceding the specific argument with the -e and -f
options; you can search for something that is in fact a file name by
using -e first (e.g. search -e /etc/passwd ~/scripts/* will find
scripts that reference /etc/passwd), and -f can be used to ensure
that a name is treated like a file.  -e and -f are also used to
ensure that something that would normally be treated like an option
or file name to search are treated like expressions and files;
e.g. "search -e -e -f -f" will search for the string "-e" within a
file named "-f".

Arguments to search.py can be:
 1) a file or directory name to search
 2) a regexp to look for in the files
 3) one of the following options:
 -e <expr> - the following argument is treated as an expression to be 
             matched (even if it matches a filename or other option)
 -f <file> - the following argument is treated as a file to be searched
             (give an error if it doesn't exist); specified filename
             may be "-" to indicate searching standard input, which
             is the default if no files are given
 -fe <expr> - the following argument is treated as an expression
              to match file names
 -v - matching is inverted; any following expressions and filename
      expressions should not match
 +v - matching is normal; any following expressions and filename
      expressions should match again (cancels a previous -v)
 -i - case insensitive matching; following expressions should match
      ignoring case
 +i - case sensitive matching; following expressions should match
      case (cancels a previous -i)
 -s - Don't interpret subsequent expressions as regular expressions but 
      just treat them as strings
 +s - treat subsequent expressions as regular expressions (cancels a 
      previous -s)
 -start <expr> - use the specified expression to denote the beginning
                 of blocks that cross line boundaries that are matched
 -end <expr> - use the specified expression to denote the end of blocks
               that cross line boundaries that are matched
display args, -dc, -df, -dl, -dn, -db
     (contents only, files only, label lines, number lines, label blocks)
 -dc - don't show filenames at all [contents]
 -df - show only filenames, not contents [fname]
 -dl - show filenames at the beginning of every line (the default for
       non-start/end processing) [fname: contents]
 -dn - like 'dl' but with lines numbered [fname:lineNumber:contents]
 -db - show filenames before each line or block of lines [fname::\\n,contents]

Filename Matching:

If any filename expressions are given to match, then files must match at
least one to be processed.  If any filename expressions are given to exclude,
then filenames matching any of those will be skipped.
'''
    if showExamples:
        helpstr += '''
Example:
   search.py myMethod ~/src -fe '[.]cc$' -fe '[.]h$' -v -fe '[0-9][.]'
   - Search only the .cc and .h files under ~/src for references to myMethod,
     but skip any files that have a digit before the period in the name.
'''
    if showDoc:
        helpstr += '''
Matching:

All files specified will be searched for all the specified
expressions.  A -v option turns off matching for all subsequent
expressions (same semantics as 'grep'), until turned back on with a
+v option and a -i option will make all the matching of all
subsequent expressions be case-insensitive (again, as 'grep' does)
until turned back on with a +i option.

File Processing:

A normal filename will be searched for the presence (or absence) of
each of the specified expressions.  If the specified file is:

a directory - then all files below it are searched
*.gz - the file is first gunzipped, then searched
*.zip - the file is first unzipped, then searched
*.tar - the file is first untarred, then all those files are searched
*.tgz - the file is first gunzipped and untarred, then all those
        files are searched
'''
    if showExamples:
        helpstr += '''
Examples:

search.py bash -v -e /home/ /etc/passwd /etc/passwd.old
  - search for users in /etc/passwd and /etc/passwd.old who use the bash
    shell and don't have a home directory in /home
search.py postfix /etc -fe '[.]conf' -df
  - find all .conf files in /etc that mention postfix and show only
    the file names.
search.py -s ".myMethod(" myArchivedProject.tar.gz -fe .java -v -fe Test.java +s "^[ \\t]*//"
  - explore the contents of the specified gzipped tar file,
    searching for calls to the method 'myMethod(' in any files with
    the archive named .java except for unit test files (named
    *Test.java), and skip any comment lines whose first
    non-whitespace characters are "//" comments.
'''
    if showDoc:
        helpstr += '''
Start/End processing:

if no -start and -end patterns are given, then blocks are
<beginning-of-line> ... <end-of-line>, and by default filenames will
be displayed at the beginning of the line unless otherwise specified.

if both a -start and -end is given, then only blocks of lines
starting with the <start> expression and ending with the <end>
expression will be searched and lines outside those regions will be
ignored.

if only a -start is given, then blocks will be file contents from
the start expression up through but not including the next start
expression. Lines prior to the first -start expression will be
skipped.

if only a -end is given, then blocks will be file contents starting after
the previous end expression up through and including the next end
expression. Lines after the final -end expression will be skipped.

If start and/or end expressions are specified, then by default, filenames
will be printed prior to the block (-db) unless otherwise specified.
'''
    if showExamples:
        helpstr += '''
Start and end expression specifications (and the related -whole
parameter) allow one to build searches such as:

mail file search:
  search.py -dc -start "\\nFrom " addr@host -v "status report" Mailfile*

  Searchs for the "addr@host" string in any multi-line block
  of the mail files, delimited by "\\nFrom " (the standard convention
  for separating mail messages in a single file) and which at the
  same time does not contain the string "status report", and
  then concatenate the results in such a way that the output is a
  valid mail file

XML block search::
   search.py -dc -start "<Person" -end "</Person>" "Joe" "manager"
       sourcedir/ -fe 'people.*[.]xml'

    Searchs all 'people*.xml' files under the sourcedir, looking for
    Person elements that contain both the string "Joe" and the
    string "manager" somewhere within any XML "Person"
    blocks in any matching files.

Whole file search
   search.py bin/ -fe .py "import.*tarfile" "import.*re" -whole -df
     find all python scripts in the bin/ directory that import the
     tarfile and re modules anywhere in the file, showing only the
     filename.
'''
    return helpstr


if __name__ == '__main__':
    main(sys.argv)

################################################################
# Dev notes:
#
# Not Yet Implemented:
#   *.Z - compressed files
#
# Option changes to make:
# TODO: add '-ds' to allow user specified display strings;
#   and then refactor the five display options to be canned default strings
#     -dc - display only matching file contents - display string "%c"
#     -df - display just filenames - display string "%f"
#     -dl - display filename with matching lines - display string "%f:%c"
#     -dn - display filename with line numbers and matching lines - display
#           string "%f:%n:%c"
#     -db - display blocks of lines with filename and line number on a separate
#           preceding line - display string "%f:%n:\n%c"
#   e.g. -ds "File %f\n\Line %n\nMatching contents:\n%c"
#
# Issues:
#   * Some open files from tarfile.extractfile() can't do 'for line in file:'
#   * expects start and end strings to be on separate lines, so will not
#     correctly searching a block like:
#     <Person><Name>Mentor</Name><Location>Arisia</Location></Person>
