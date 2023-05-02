import java.util.Arrays;
import java.util.List;
import java.util.Scanner;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class JavaLambdaVersion {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);

        int N = scanner.nextInt();
        int M = scanner.nextInt();
        scanner.nextLine();

        int[][] board = IntStream.range(0, N)
                                 .mapToObj(i -> scanner.nextLine().chars().map(c -> c == '#' ? 1 : 0).toArray())
                                 .toArray(int[][]::new);

        int[][] block = IntStream.range(0, 3)
                                 .mapToObj(i -> scanner.nextLine().chars().map(c -> c == '#' ? 1 : 0).toArray())
                                 .toArray(int[][]::new);

        if (canInsertBlock(board, block)) {
            System.out.println("Yes");
        } else {
            System.out.println("No");
        }
    }

    private static boolean canPlaceBlockAt(int[][] board, int[][] block, int x, int y) {
        return IntStream.range(0, 3)
                        .allMatch(i ->
                            IntStream.range(0, 3)
                                     .allMatch(j -> block[i][j] != 1 || (x + i < board.length && y + j < board[0].length && board[x + i][y + j] == 0))
                        );
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
        List<int[][]> rotations = Stream.iterate(block, b -> b != null, JavaLambdaVersion::rotateBlock)
                                        .limit(4)
                                        .collect(Collectors.toList());

        return IntStream.range(0, board.length)
                        .anyMatch(x ->
                            IntStream.range(0, board[0].length)
                                     .anyMatch(y ->
                                         rotations.stream().anyMatch(rotatedBlock -> canPlaceBlockAt(board, rotatedBlock, x, y))
                                     )
                        );
    }
}
