CC = gcc
CFLAGS = -Wall -Wextra -std=c11 -g

all: main

tinyc: main.c
	$(CC) $(CFLAGS) -o main main.c

# テスト: hello.tc をコンパイルして実行
test: main
	./main main.tc -o hello.s
	gcc -o hello hello.s
	./hello && echo "TEST PASSED (exit 0)" || echo "TEST FAILED"

# アセンブリ確認
asm: main
	./main hello.tc -o hello.s
	cat hello.s

clean:
	rm -f main hello hello.s test_out.s
