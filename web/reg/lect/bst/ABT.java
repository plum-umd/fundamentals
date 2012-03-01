// Common code to implementations of IBT<X>.
public abstract class ABT<X> implements IBT<X> {

    // Number of values in this tree.
    public Integer size() {
	return this.accept(new Size<X>());
    }
}
