define('widget', ["util"], function(util) {
function Widget(obj, $obj, owner) {
    var self = this;
    this.$obj = $obj;
    obj.$self = $obj;
    obj.widget = this;
    this.obj = obj;
    this.children = [];
    this.owner = owner;
    if (owner) {
        owner.children.push(this);
    }
    obj.$self.mousedown(util.guard(function(ev) {
        self.objForward(ev, 'mousedown');
    }));
    obj.$self.mouseup(util.guard(function(ev) {
        self.objForward(ev, 'mouseup');
    }));
    obj.$self.mousemove(util.guard(function(ev) {
        self.objForward(ev, 'mousemove');
    }));
    obj.$self.click(util.guard(function(ev) {
        self.objForward(ev, 'click');
    }));
}
Widget.prototype.objForward = util.guard(function Widget_objForward(ev, name) {
    if (this.obj[name]) {
        this.obj[name](ev);
    }
})
Widget.prototype.actualWidth = function Widget_actualWidth() {
    return this.$obj.outerWidth(true);
}

return Widget;
});
