import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

class Problem1 {
    public static void main(String[] args) {
        Scanner s = null;
        try {
            s = new Scanner(new File("input.txt"));
        } catch(FileNotFoundException e) {
            System.err.println(e);
            System.exit(1);
        }
        int sum = 0;

        while(s.hasNextLine()) {
            String line = s.nextLine();
            char op = line.charAt(0);
            int num = Integer.parseInt(line.substring(1));
            if(op == '+')
                sum += num;
            else 
                sum -= num;
        }

        System.out.println(sum);
    }
}