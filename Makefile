##
## EPITECH PROJECT, 2023
## FUN/ImageCompressor
## File description:
## Makefile
##

OBJ	=	$(shell stack path --local-install-root)

TEST_PATH	=	 $(shell stack path --local-hpc-root)

OUTPUT_PATH	=	test/coverage

NAME	=	imageCompressor

all:	$(NAME)
.PHONY:	all

$(NAME):
	@stack build
	@cp $(OBJ)/bin/$(NAME)-exe ./$(NAME)

clean:
	@stack clean
.PHONY: clean

fclean:
	@stack purge
	@rm -rf $(NAME)
.PHONY: fclean

tests_run:
	@mkdir -p $(OUTPUT_PATH)
	@stack test --coverage
	@cp $(TEST_PATH)/$(NAME)/$(NAME)-test/$(NAME)-test.tix $(OUTPUT_PATH)
.PHONY: tests_run

re: fclean all
.PHONY: re
