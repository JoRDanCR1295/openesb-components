/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package bpelmonitor.svg;

import com.icesoft.faces.component.ext.HtmlCommandLink;
import javax.faces.component.UIComponent;
import javax.faces.event.ActionEvent;

/**
 *
 * @author mbhasin
 */
public class SVG {

    private String fileName = "./resources/svg/AsynchronousSample.svg";
    private String width = "600";
    private String height = "300";

    /**
     * @return the fileName
     */
    public String getFileName() {
        return fileName;
    }

    /**
     * @param fileName the fileName to set
     */
    public void setFileName(String fileName) {
        this.fileName = fileName;
    }

    /**
     * @return the width
     */
    public String getWidth() {
        return width;
    }

    /**
     * @param width the width to set
     */
    public void setWidth(String width) {
        this.width = width;
    }

    /**
     * @return the height
     */
    public String getHeight() {
        return height;
    }

    /**
     * @param height the height to set
     */
    public void setHeight(String height) {
        this.height = height;
    }

    public void submitButtonListener(ActionEvent event) {
        UIComponent comp = event.getComponent();
        if (comp instanceof HtmlCommandLink) {
            HtmlCommandLink commandLink = (HtmlCommandLink) event.getComponent();
            String value = (String) commandLink.getValue();
            if (value.equals("Asyncronous BP")) {
                this.fileName = "AsynchronousSample.svg";
                this.width = "600";
                this.height = "300";
            } else if (value.equals("Travel Reservation")) {
                this.fileName = "TravelReservationService.svg";
                this.width = "2000";
                this.height = "600";
            }
        }
    }
}
