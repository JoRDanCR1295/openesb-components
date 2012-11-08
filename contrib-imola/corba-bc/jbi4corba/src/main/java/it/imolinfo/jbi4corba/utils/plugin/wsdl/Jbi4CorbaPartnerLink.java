/*
 * Jbi4CorbaPartnerLink.java
 * 
 * Created on 12-set-2007, 16.50.46
 * 
 * This class represents the PartnerLink 
 */

package it.imolinfo.jbi4corba.utils.plugin.wsdl;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

/**
 *
 * @author mcastaldi
 */
public class Jbi4CorbaPartnerLink implements ExtensibilityElement, Serializable {

    private Boolean required = false;
    private QName elementType = null;
    private String name = null;
    private List<Role> roles = new ArrayList<Role>();
    private String prefix = null;
    

    public void setElementType(QName elementType) {
        this.elementType = elementType;
    }

    public QName getElementType() {
        return this.elementType;
    }

    public void setRequired(Boolean required) {
        this.required = required;
    }

    public Boolean getRequired() {
        return this.required;
    }

    public void addRole(Role role) {
        this.roles.add(role);
    }

    public void removeRole(Role role) {
        this.roles.remove(role);
    }

    public Role getRole(int i) {
        return this.roles.get(i);
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public int getRolesNumber() {
        return this.roles.size();
    }

    public String getPrefix() {
        return prefix;
    }

    public void setPrefix(String prefix) {
        this.prefix = prefix;
    }

}
