int main() {
    for (int i = 0;
	 i < j &&		// Bogus analysis.
	     i < 17;		// Bogus.
	 i++) {}
}
