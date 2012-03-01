public abstract class ABT<X> implements IBT<X> {
    public Integer size() {
	return this.accept(new Size<X>());
    }
}
