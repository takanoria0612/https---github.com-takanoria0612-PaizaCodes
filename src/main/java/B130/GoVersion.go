package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	scanner := bufio.NewScanner(os.Stdin)

	scanner.Scan()
	nm := strings.Split(scanner.Text(), " ")
	N, _ := strconv.Atoi(nm[0])

	board := readBoard(scanner, N)
	block := readBlock(scanner)

	if canInsertBlock(board, block) {
		fmt.Println("Yes")
	} else {
		fmt.Println("No")
	}
}

func readBoard(scanner *bufio.Scanner, N int) [][]int {
	board := make([][]int, N)
	for i := 0; i < N; i++ {
		scanner.Scan()
		line := scanner.Text()
		board[i] = make([]int, len(line))
		for j, c := range line {
			if c == '#' {
				board[i][j] = 1
			} else {
				board[i][j] = 0
			}
		}
	}
	return board
}

func readBlock(scanner *bufio.Scanner) [][]int {
	block := make([][]int, 3)
	for i := 0; i < 3; i++ {
		scanner.Scan()
		line := scanner.Text()
		block[i] = make([]int, len(line))
		for j, c := range line {
			if c == '#' {
				block[i][j] = 1
			} else {
				block[i][j] = 0
			}
		}
	}
	return block
}

func canInsertBlock(board, block [][]int) bool {
	for i := 0; i < 4; i++ {
		for j := 0; j < len(board); j++ {
			for k := 0; k < len(board[0]); k++ {
				if canPlaceBlockAt(board, block, j, k) {
					return true
				}
			}
		}
		block = rotateBlock(block)
	}
	return false
}

func canPlaceBlockAt(board, block [][]int, x, y int) bool {
	for i := 0; i < 3; i++ {
		for j := 0; j < 3; j++ {
			if block[i][j] == 1 {
				if x+i >= len(board) || y+j >= len(board[0]) || board[x+i][y+j] == 1 {
					return false
				}
			}
		}
	}
	return true
}

func rotateBlock(block [][]int) [][]int {
	rotated := make([][]int, 3)
	for i := 0; i < 3; i++ {
		rotated[i] = make([]int, 3)
	}
	for i := 0; i < 3; i++ {
		for j := 0; j < 3; j++ {
			rotated[j][2-i] = block[i][j]
		}
	}
	return rotated
}
