package ru.spbau.turaevT.cw3;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.*;
import java.text.MessageFormat;

public class JavaGenerator {
    private final Class<?> classInfo;

    public JavaGenerator(Class<?> classInfo) {
        this.classInfo = classInfo;
    }

    public String generateJavaFile() throws IOException {
        StringBuilder sb = new StringBuilder();

        writeHeader(sb);
        writeFields(sb);
        writeConstructors(sb);
        writeMethods(sb);
        writeFooter(sb);

        String filename = MessageFormat.format("{0}.java", classInfo.getSimpleName());

        try (BufferedWriter bufferedWriter = new BufferedWriter(new FileWriter(filename))) {
            bufferedWriter.write(sb.toString());
        }
        return filename;
    }

    private void writeFooter(StringBuilder sb) {
        sb.append("}");
    }

    private void writeMethods(StringBuilder sb) {
        for (Method method : classInfo.getDeclaredMethods()) {
            sb.append(MessageFormat.format("{0} {1} {2} (", getMemberModifiers(method), method.getReturnType().getSimpleName(), method.getName()));
            writeParams(sb, method.getParameterTypes());
            sb.append(MessageFormat.format(") '{'{0}    {1}{0}'}'{0}{0}", System.lineSeparator(), getReturnExpression(method.getReturnType())));
        }
    }

    private String getReturnExpression(Class<?> returnType) {
        String defaultValue = getDefaultValue(returnType);
        if (defaultValue.isEmpty())
            return "";
        else return "return " + defaultValue + ";";
    }

    private String getDefaultValue(Class<?> returnType) {
        if (!returnType.isPrimitive() || returnType.equals(String.class))
            return "null";
        if (returnType.equals(void.class)) {
            return "";
        }
        if (returnType.equals(boolean.class)) {
            return "false";
        }
        return "0";
    }

    private void writeFields(StringBuilder sb) {
        for (Field field : classInfo.getDeclaredFields()) {
            sb.append(MessageFormat.format("{0} {1} {2}{4};{3}{3}", getMemberModifiers(field), field.getType().getSimpleName(),
                    field.getName(), System.lineSeparator(), getInitializationFinalField(field)));
        }
    }

    private String getInitializationFinalField(Field field) {
        if (!Modifier.isFinal(field.getModifiers()))
            return "";
        return " = " + getDefaultValue(field.getType());
    }

    private void writeConstructors(StringBuilder sb) {
        for (Constructor<?> constructor : classInfo.getDeclaredConstructors()) {
            sb.append(MessageFormat.format("{0} {1} (", getMemberModifiers(constructor), classInfo.getSimpleName()));
            Class<?>[] parameterTypes = constructor.getParameterTypes();
            writeParams(sb, parameterTypes);
            sb.append(MessageFormat.format(") '{'{0}'}'{0}", System.lineSeparator()));
        }
    }

    private void writeParams(StringBuilder sb, Class<?>[] parameterTypes) {
        for (int i = 0; i < parameterTypes.length; i++) {
            sb.append(MessageFormat.format("{2} {0} obj{1}", parameterTypes[i].getSimpleName(), i, Modifier.isFinal(parameterTypes[i].getModifiers()) ? "final" : ""));
        }
    }

    private void writeHeader(StringBuilder sb) {
        sb.append(MessageFormat.format(
                "import java.lang.*;" + System.lineSeparator()
                        + "{0} class {1} '{' {2}", getModifiers(), classInfo.getSimpleName(), System.lineSeparator()
        ));
    }

    private String getModifiers() {
        return Modifier.toString(classInfo.getModifiers());
    }

    private String getMemberModifiers(Member member) {
        return Modifier.toString(member.getModifiers());
    }
}
