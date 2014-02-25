package ru.spbau.turaevT.task1;

/**
 * Entry Point
 * <p/>
 * Accumulates passed as command line arguments integers and prints sum to console
 *
 * @author Timur Turaev
 * @version 1.0
 */
public class Sum {
    /**
     * Prints sum of command line arguments, interpreting as numbers
     *
     * @param args Command line arguments
     */
    public static void main(String[] args) {
        int result = 0;

        for (String arg : args) {
            result += getSumOfNumbersInString(arg);
        }
        System.out.println(result);
    }

    private static int getSumOfNumbersInString(String str) {
        int stringLength = str.length();
        int leftIndex = 0;
        int result = 0;

        while (leftIndex < stringLength) {
            while (Character.isWhitespace(str.charAt(leftIndex))) {
                leftIndex++;
                if (leftIndex >= stringLength) {
                    return result;
                }
            }

            int rightIndex = leftIndex + (str.charAt(leftIndex) == '-' ? 1 : 0);
            while (rightIndex < stringLength && Character.isDigit(str.charAt(rightIndex))) {
                rightIndex++;
            }

            result += convertStringToInteger(str.substring(leftIndex, rightIndex));
            leftIndex = rightIndex;
        }
        return result;
    }

    private static int convertStringToInteger(String str) {
        int stringLength = str.length();
        int i = 0;
        int result = 0;

        int sign = 1;
        if (i < stringLength && str.charAt(i) == '-') {
            sign = -1;
            i++;
        }

        for (; i < stringLength; i++) {
            result = result * 10 + sign * (str.charAt(i) - '0');
        }
        return result;
    }
}
