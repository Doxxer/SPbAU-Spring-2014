package ru.spbau.turaevT.cw3;

import javax.tools.*;
import java.io.IOException;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.Callable;

public class Checker implements Callable<List<String>> {

    private final Class<?> classInfo;

    public Checker(Class<?> classInfo) {

        this.classInfo = classInfo;
    }

    @Override
    public List<String> call() {
        try {
            String filename = new JavaGenerator(classInfo).generateJavaFile();
            return isCompile(filename);
        } catch (IOException e) {
            System.err.println(MessageFormat.format("IOException occurred: {0}", e.getMessage()));
            return new ArrayList<>();
        }
    }

    private List<String> isCompile(String filename) throws IOException {
        JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
        DiagnosticCollector<JavaFileObject> diagnostics = new DiagnosticCollector<>();

        StandardJavaFileManager fileManager = compiler.getStandardFileManager(diagnostics, null, null);
        Iterable<? extends JavaFileObject> compilationUnits = fileManager.getJavaFileObjectsFromStrings(Arrays.asList(filename));
        JavaCompiler.CompilationTask task = compiler.getTask(null, fileManager, diagnostics, null, null, compilationUnits);
        boolean success = task.call();
        fileManager.close();

        List<String> result = new ArrayList<>();
        if (!success) {
            for (Diagnostic<? extends JavaFileObject> diagnostic : diagnostics.getDiagnostics()) {
                result.add(diagnostic.toString());
            }
        }
        return result;
    }
}
