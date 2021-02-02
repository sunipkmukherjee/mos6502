CC=gcc
RM=rm -rvf

MAINFILE=c_6502.c
EXEC=test.out

all:
	$(CC) -I./ -DUNIT_TEST $(MAINFILE) -o $(EXEC)
	./$(EXEC)

clean:
	$(RM) $(EXEC)