package {
    import flash.display.Sprite;
    import flash.text.TextField;
    public class textField extends Sprite {
        public function textField() {
            super();
	    var field : TextField = new TextField();
	    field.text = "Hello, World!";
	    addChild(field);
        }
    }
}
