package ru.spbau.turaevT.montecarlo;

import java.util.Random;

public class Main {
    private static final int N = 1000000;

    private static Random random;

    private static double getInclusive() {
        double v1 = random.nextDouble();
        double v2 = 1.0f - random.nextDouble();
        return random.nextBoolean() ? v1 : v2;
    }

    private static double getRandom(int a, int b) {
        return a + getInclusive() * (b - a);
    }

    public static void main(String[] args) {
        random = new Random();

        double sum = 0;
        for (int i = 0; i < N; i++) {
            sum += ƒ(getRandom(0, 1));
        }
        System.out.println(sum / N);
    }

    private static double ƒ(double x) {
        return Math.sin(x + Math.exp(-x));
    }
}
