
import javax.faces.event.ValueChangeEvent;

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author mbhasin
 */
public class PanelBorderBean {

  // five configurable areas that make up a BorderLayout
    private boolean renderNorth = false;
    private boolean renderSouth = false;
    private boolean renderCenter = false;
    private boolean renderEast = true;
    private boolean renderWest = true;

    /**
     * event handler for the north border.
     *
     * @param event the value change event.
     */
    public void north(ValueChangeEvent event) {
        setRenderNorth(((Boolean) event.getNewValue()).booleanValue());
    }


    /**
     * event handler for the south border.
     *
     * @param event the value change event.
     */
    public void south(ValueChangeEvent event) {
        setRenderSouth(((Boolean) event.getNewValue()).booleanValue());
    }

    /**
     * event handler for the center border.
     *
     * @param event the value change event.
     */
    public void center(ValueChangeEvent event) {
        setRenderCenter(((Boolean) event.getNewValue()).booleanValue());
    }

    /**
     * event handler for the east border.
     *
     * @param event the value change event.
     */
    public void east(ValueChangeEvent event) {
        setRenderEast(((Boolean) event.getNewValue()).booleanValue());
    }

    /**
     * event handler for the west border.
     *
     * @param event the value change event.
     */
    public void west(ValueChangeEvent event) {
        setRenderWest(((Boolean) event.getNewValue()).booleanValue());
    }

    public boolean isRenderCenter() {
        return renderCenter;
    }

    public void setRenderCenter(boolean renderCenter) {
        this.renderCenter = renderCenter;
    }

    public boolean isRenderEast() {
        return renderEast;
    }

    public void setRenderEast(boolean renderEast) {
        this.renderEast = renderEast;
    }

    public boolean isRenderNorth() {
        return renderNorth;
    }

    public void setRenderNorth(boolean renderNorth) {
        this.renderNorth = renderNorth;
    }

    public boolean isRenderSouth() {
        return renderSouth;
    }

    public void setRenderSouth(boolean renderSouth) {
        this.renderSouth = renderSouth;
    }

    public boolean isRenderWest() {
        return renderWest;
    }

    public void setRenderWest(boolean renderWest) {
        this.renderWest = renderWest;
    }
}
