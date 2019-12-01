import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;
import java.util.Set;
import java.util.HashSet;

class Problem2 {
    public static void main(String[] args) {
        try {
            Scanner s = new Scanner(new File("input.txt"));

            int sum = 0;
            Set<Integer> freqs = new HashSet<Integer>();

            while (true) {
                String line = s.nextLine();
                char op = line.charAt(0);
                int num = Integer.parseInt(line.substring(1));
                if (op == '+')
                    sum += num;
                else
                    sum -= num;

                if (!freqs.add(sum)) {
                    break;
                }

                if (!s.hasNextLine()) {
                    s = new Scanner(new File("input.txt"));
                }
            }

            System.out.println(sum);
        } catch (FileNotFoundException e) {
            System.err.println(e);
            System.exit(1);
        }
    }
}