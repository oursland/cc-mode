/* NEW_MAPPING_LOOP below is a macro.  It's recognized as a nested
 * defun, which while not really correct still gives sane indentation.
 * (Besides, for all we know the macro could expand to to a defun as
 * well.) */
void x()
{
    if (sp[-1].type == T_MAPPING)
	if (sp[-1].u.mapping->md) {
	    foo();
	}
}
void y()
{
    if (sp[-1].type == T_MAPPING)
	NEW_MAPPING_LOOP (sp[-1].u.mapping->md) {
	    foo();
	}
}
void z()
{
    if (sp[-1].type == T_MAPPING)
	NEW_MAPPING_LOOP (sp[-1].u.mapping->md)
	{
	    foo();
	}
}
