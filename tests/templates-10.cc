template <Polarization P>
struct AtomTerm
{
    virtual ~AtomTerm();
    virtual AtomComp<P>* get_atom() const = 0;
};

// Local Variables:
// cc-test-skip: (no-syntax-properties)
// End:
