void f() {
    rx = Rx.Rx (Rx.or (@map (lambda (int alt) {
				 return Rx.save (({sub, 'b' + alt}));
			     },
			     reverse (indices (allocate (alts))))));
}
