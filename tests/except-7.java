public class Test {
    public static void main(String[] args) {
        Path dir = Paths.get(System.getProperty("user.home"));
        try (DirectoryStream<Path> stream = Files.newDirectoryStream(dir)) {
            System.out.println("test");
            }
    }
}
