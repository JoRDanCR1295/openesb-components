/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package it.imolinfo.jbi4corba.jbi.wsdl;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.apache.commons.lang.builder.ReflectionToStringBuilder;

/**
 *
 * @author raffaele
 */
public class Jbi4CorbaIDLEntry {
    private String IDL;
    private String idlFilename=Jbi4CorbaExtension.DEFAULT_FILENAME;
    private String relativePath=Jbi4CorbaExtension.DEFAULT_RELATIVE_PATH;
    private boolean root=Jbi4CorbaExtension.DEFAULT_ROOT;

    public static final Jbi4CorbaIDLEntry DEFAULT_CORBA_IDL_ENTRY=new Jbi4CorbaIDLEntry();

    static {
       DEFAULT_CORBA_IDL_ENTRY.setIdlFilename(Jbi4CorbaExtension.DEFAULT_FILENAME);
       DEFAULT_CORBA_IDL_ENTRY.setRelativePath(Jbi4CorbaExtension.DEFAULT_RELATIVE_PATH);
       DEFAULT_CORBA_IDL_ENTRY.setRoot(Jbi4CorbaExtension.DEFAULT_ROOT);
    }

    public Jbi4CorbaIDLEntry() {
    }

    public String getIDL() {
        return IDL;
    }

    public void setIDL(String IDL) {
        this.IDL = IDL;
    }

    public String getIdlFilename() {
        return idlFilename;
    }

    public void setIdlFilename(String idlFilename) {
        this.idlFilename = idlFilename;
    }

    public String getRelativePath() {
        return relativePath;
    }

    public void setRelativePath(String relativePath) {
        this.relativePath = relativePath;
    }

    public boolean isRoot() {
        return root;
    }

    public void setRoot(boolean root) {
        this.root = root;
    }

        /**
     * @return The return
     */
    public String toString() {
        return ReflectionToStringBuilder.toString(this);
    }

    /**
     * @param obj
     *            The object
     * @return The return
     */
    public boolean equals(Object obj) {
        return EqualsBuilder.reflectionEquals(this, obj);
    }

    /**
     * @return The return
     */
    public int hashCode() {
        return HashCodeBuilder.reflectionHashCode(17, 37, this);
    }

    public boolean isDefault(){
        return DEFAULT_CORBA_IDL_ENTRY.root==this.root && DEFAULT_CORBA_IDL_ENTRY.idlFilename.equals(this.idlFilename) && DEFAULT_CORBA_IDL_ENTRY.relativePath.equals(this.relativePath);
    }



}
