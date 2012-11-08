
import javax.faces.event.ValueChangeEvent;

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author mbhasin
 */
public class TextFieldsBean {
    /**
     * The different kinds of text input fields.
     */
    private String name;
    private String password;
    private String comments;

    /**
     * Gets the name property.
     *
     * @return value of name property
     */
    public String getName() {
        return name;
    }

    /**
     * Sets the name property
     *
     * @param newValue new value of the name property
     */
    public void setName(String newValue) {
        name = newValue;
    }

    /**
     * Gets the password property.
     *
     * @return value of the password property
     */
    public String getPassword() {
        return password;
    }

    /**
     * Sets the password property.
     *
     * @param newValue new value of the password property
     */
    public void setPassword(String newValue) {
        password = newValue;
    }

    /**
     * Gets the comments property.
     *
     * @return value of the comments property
     */
    public String getComments() {
        return comments;
    }

    /**
     * Sets the comments property.
     *
     * @param newValue new value of the comments property
     */
    public void setComments(String newValue) {
        comments = newValue;
    }

    public void effectChangeListener(ValueChangeEvent event) {
        
    }
}
