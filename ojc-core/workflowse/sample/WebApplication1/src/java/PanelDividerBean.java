/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
import javax.faces.event.ValueChangeEvent;
import java.io.Serializable;

/**
 * Simple model bean for the panelDivider example.  The bean maintains the
 * position and orientation of the divider.
 *
 * @since 1.7
 */
public class PanelDividerBean  implements Serializable {

    public static final String ORIENTATION_HOR = "horizontal";
    public static final String ORIENTATION_VER = "vertical";

    private static final int POSITION_DEFAULT = 45;

    private String orientation = ORIENTATION_VER;
    private int position = POSITION_DEFAULT;

    public String getOrientation() {
        return orientation;
    }

    public int getPosition() {
        return position;
    }

    public void setOrientation(String orientation) {
        this.orientation = orientation;
    }

    public void setPosition(int position) {
        this.position = position;
    }

    /**
     * Listener method called when the orientation is changed
     * This is useful to allow us to reset the position of the divider to
     * the default value
     *
     * @param event of the change jsf event
     */
    public void orientationChanged(ValueChangeEvent event) {
        this.position = POSITION_DEFAULT;
    }
}
