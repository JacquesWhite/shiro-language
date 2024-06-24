# Tools

BNFC       = bnfc
BNFC_OPTS  = --haskell -d --functor
GHC        = ghc
GHC_OPTS   = -Wall -Werror -Wextra -O2
HAPPY      = happy
HAPPY_OPTS = --array --info --ghc --coerce
ALEX       = alex
ALEX_OPTS  = --ghc

# Directories
SRC_DIR = Src
GHC_TARGET_DIR = Target
GRAMMAR_DIR = Grammar
TESTCASES_BAD_DIR = bad
TESTCASES_GOOD_DIR = good
TEST_RESOURCES_DIR = TestResources

BNFC_DIR = $(SRC_DIR)/Shiro
TYPECHECKER_DIR = $(SRC_DIR)/TypeChecker
TYPECHECKER_TEST_DIR = $(TYPECHECKER_DIR)/Test
EVALUATOR_DIR = $(SRC_DIR)/Evaluator
GRAMMAR_DIR = Grammar


# Files
GRAMMAR_FILE = $(GRAMMAR_DIR)/Shiro.cf


ABS_SYNTAX_SRC = $(BNFC_DIR)/Abs.hs
PARSER_SRC = $(BNFC_DIR)/Par.hs
LEXER_SRC = $(BNFC_DIR)/Lex.hs

TYPECHECKER_TEST_SRC = $(TYPECHECKER_TEST_DIR)/TypeCheckerExceptionsTest.hs
TYPECHECKER_SRCS = $(wildcard $(SRC_DIR)/TypeChecker/*.hs)
SRC_COMMON_SRCS = $(wildcard $(SRC_DIR)/Common/*.hs)
SRC_EVALUATOR_SRCS = $(wildcard $(SRC_DIR)/Evaluator/*.hs)


INTERPRETER_SRC =\
	$(SRC_DIR)/Interpreter.hs

# Phony targets
.PHONY : all typeChecker release clean cleantarget cleaninterpreter cleantest cleantypechecker

# Default goal
all : interpreter

interpreter : $(INTERPRETER_SRC) $(SRC_EVALUATOR_SRCS) $(TYPECHECKER_SRCS) $(SRC_COMMON_SRCS) $(PARSER_SRC) $(ABS_SYNTAX_SRC) 
	$(GHC) $(GHC_OPTS) -odir $(GHC_TARGET_DIR) -hidir $(GHC_TARGET_DIR) $(SRC_DIR)/Interpreter -o interpreter

typeCheckerTest : $(TYPECHECKER_TEST_SRC) $(TYPECHECKER_SRCS) $(SRC_COMMON_SRCS) $(PARSER_SRC) $(ABS_SYNTAX_SRC)
	$(GHC) $(GHC_OPTS) -odir $(GHC_TARGET_DIR) -hidir $(GHC_TARGET_DIR) $< -o typeCheckerTest

typeChecker : $(PARSER_SRC)

PARSER_TEST_SRC = $(BNFC_DIR)/Test.hs
parserTest : $(PARSER_TEST_SRC) $(PARSER_SRC) $(LEXER_SRC) $(ABS_SYNTAX_SRC)
	$(GHC) $(GHC_OPTS) -odir $(GHC_TARGET_DIR) -hidir $(GHC_TARGET_DIR) $< -o parserTest

PARSER_PREREQUISITE = $(BNFC_DIR)/Par.y
$(PARSER_SRC) : $(PARSER_PREREQUISITE) $(BNFC_GOALS)
	$(HAPPY) $(HAPPY_OPTS) $<

LEXER_REREQUISITE = $(BNFC_DIR)/Lex.x
$(LEXER_SRC) : $(LEXER_REREQUISITE)
	$(ALEX) $(ALEX_OPTS) $<

$(ABS_SYNTAX_SRC) : $(LEXER_SRC) 

BNFC_GOALS =\
	$(ABS_SYNTAX_SRC)\
	$(LEXER_REREQUISITE)\
	$(PARSER_PREREQUISITE)\
	$(PARSER_TEST_SRC)
$(BNFC_GOALS) : $(GRAMMAR_FILE)
	$(BNFC) $(BNFC_OPTS) -p $(SRC_DIR) $<


RELEASE_DIR = release
STUDENT_ID = jn418433
RELEASE_TMP_DIR = $(RELEASE_DIR)/$(STUDENT_ID)

release : clean
	rm -rf $(RELEASE_TMP_DIR)
	rm -f $(RELEASE_DIR)/$(STUDENT_ID).zip
	mkdir -p $(RELEASE_TMP_DIR)
	cp -r $(SRC_DIR) $(RELEASE_TMP_DIR)
	cp -r $(GRAMMAR_DIR) $(RELEASE_TMP_DIR)
	cp -r $(TESTCASES_BAD_DIR) $(RELEASE_TMP_DIR)
	cp -r $(TESTCASES_GOOD_DIR) $(RELEASE_TMP_DIR)
	cp -r $(TEST_RESOURCES_DIR) $(RELEASE_TMP_DIR)
	cp Makefile $(RELEASE_TMP_DIR)
	cp README.md $(RELEASE_TMP_DIR)
	cp test.sh $(RELEASE_TMP_DIR)
	cd $(RELEASE_DIR) && zip -r $(STUDENT_ID).zip $(STUDENT_ID)
	rm -rf $(RELEASE_TMP_DIR)


# Rules for cleaning generated files.
clean : cleantarget cleantest cleaninterpreter cleanparsertest cleantypechecker cleantmprelease

cleantarget :
	-rm -rf $(GHC_TARGET_DIR)

cleaninterpreter :
	-rm -rf interpreter

cleantest :
	-rm -rf typeCheckerTest

cleanparsertest :
	-rm -rf parserTest

cleantypechecker :
	-rm -rf $(BNFC_DIR)

cleantmprelease :
	-rm -rf $(RELEASE_TMP_DIR)
