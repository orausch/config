const { Gio } = imports.gi;

const MR_DBUS_IFACE = `
<node>
    <interface name="org.gnome.Shell.Extensions.OrauschMods">
        <method name="Focus">
            <arg type="s" direction="in" name="wmclass"/>
        </method>
        <method name="Call">
            <arg type="s" direction="in" name="wmclass"/>
            <arg type="u" direction="in" name="workspace"/>
            <arg type="u" direction="in" name="x"/>
            <arg type="u" direction="in" name="y"/>
            <arg type="u" direction="in" name="width"/>
            <arg type="u" direction="in" name="height"/>
        </method>
    </interface>
</node>`;

class Extension {
    enable() {
        this._dbus = Gio.DBusExportedObject.wrapJSObject(MR_DBUS_IFACE, this);
        this._dbus.export(Gio.DBus.session, '/org/gnome/Shell/Extensions/OrauschMods');
    }

    disable() {
        this._dbus.flush();
        this._dbus.unexport();
        delete this._dbus;
    }

    Focus(wmclass) {
        let win = global.get_window_actors()
            .map(a => a.meta_window)
            .map(w => ({ class: w.get_wm_class(), title: w.get_title(), ws: w }))
            .find(ws => ws.class == wmclass);
        if (win) {
            win.ws.focus(0);
            win.ws.raise();
            return true;
        } else {
            throw new Error('Not found');
        }
    }

    Call(wmclass, workspace, x, y, width, height) {
        let win = global.get_window_actors()
            .map(a => a.meta_window)
            .map(w => ({ class: w.get_wm_class(), title: w.get_title(), ws: w }))
            .find(ws => ws.class == wmclass);
        if (win) {
            win.ws.change_workspace_by_index(workspace, false);
            win.ws.move_resize_frame(0, x, y, width, height);
        } else {
            throw new Error('Not found');
        }
    }
}

function init() {
    return new Extension();
}
