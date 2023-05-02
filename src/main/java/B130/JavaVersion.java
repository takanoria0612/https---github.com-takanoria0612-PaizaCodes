package src.main.java.B130;

import java.util.Scanner;

public class JavaVersion {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);

        int N = scanner.nextInt();
        int M = scanner.nextInt();
        scanner.nextLine();

        int[][] board = new int[N][M];
        for (int i = 0; i < N; i++) {
            String line = scanner.nextLine();
            for (int j = 0; j < M; j++) {
                board[i][j] = line.charAt(j) == '#' ? 1 : 0;
            }
        }

        int[][] block = new int[3][3];
        for (int i = 0; i < 3; i++) {
            String line = scanner.nextLine();
            for (int j = 0; j < 3; j++) {
                block[i][j] = line.charAt(j) == '#' ? 1 : 0;
            }
        }

        if (canInsertBlock(board, block)) {
            System.out.println("Yes");
        } else {
            System.out.println("No");
        }
    }

    private static boolean canPlaceBlockAt(int[][] board, int[][] block, int x, int y) {
        for (int i = 0; i < 3; i++) {
            for (int j = 0; j < 3; j++) {
                if (block[i][j] == 1) {
                    if (x + i >= board.length || y + j >= board[0].length || board[x + i][y + j] == 1) {
                        return false;
                    }
                }
            }
        }
        return true;
    }

    private static int[][] rotateBlock(int[][] block) {
        int[][] rotated = new int[3][3];
        for (int i = 0; i < 3; i++) {
            for (int j = 0; j < 3; j++) {
                rotated[j][2 - i] = block[i][j];
            }
        }
        return rotated;
    }

    private static boolean canInsertBlock(int[][] board, int[][] block) {
        for (int i = 0; i < 4; i++) {
            for (int j = 0; j < board.length; j++) {
                for (int k = 0; k < board[0].length; k++) {
                    if (canPlaceBlockAt(board, block, j, k)) {
                        return true;
                    }
                }
            }
            block = rotateBlock(block);
        }
        return false;
    }
}

