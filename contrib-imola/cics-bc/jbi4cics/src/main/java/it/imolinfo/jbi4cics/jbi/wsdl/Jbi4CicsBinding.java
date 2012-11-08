/*
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 */


package it.imolinfo.jbi4cics.jbi.wsdl;

import java.io.Serializable;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;
import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.apache.commons.lang.builder.ReflectionToStringBuilder;

/**
 * The bean representing the WSDL binding extension.
 *
 * @author amedeocannone
 * @author marcopiraccini
 * @author <a href="mailto:mcimatti@imolinfo.it">Marco Cimatti</a>
 */
public class Jbi4CicsBinding implements ExtensibilityElement, Serializable {

    /**
     * The generated serial version UID.
     */
    private static final long serialVersionUID = 538349432116207249L;

    /**
     * The element type.
     */
    private QName elementType;

    /**
     * The required flag.
     */
    private Boolean required;

    /**
     * The input copy Cobol. May be the same as the output copy Cobol or it may
     * be a different one.
     */
    private String copyCobol;

    /**
     * The output copy Cobol. May be the same as the input copy Cobol or it may
     * be a different one.
     */
    private String outputCopyCobol;

    /**
     * Flag indicating if the input copy Cobol and the output copy Cobol are the
     * same. If set to <code>Boolean.FALSE</code>, then also the
     * {@link #outputCopyCobol} is used.
     */
    private Boolean sameCopyCobol;

    /**
     * The code page.
     */
    private String codePage;

    /**
     * The service package name.
     */
    private String svcPackageName;

    /**
     * Does nothing.
     */
    public Jbi4CicsBinding() {
    }

    /**
     * Returns a string representation of this object, showing all internal
     * values.
     *
     * @return  a string representation of this object.
     */
    @Override
    public String toString() {                                  // Overridden
        return ReflectionToStringBuilder.toString(this);
    }

    /**
     * Indicates whether some other object is "equal to" this one, comparing its
     * internal values.
     *
     * @param   obj   the reference object with which to compare.
     * @return  <code>true</code> if this object is the same class as the obj
     *          argument and contains the same internal values;
     *          <code>false</code> otherwise.
     */
    @Override
    public boolean equals(Object obj) {                         // Overridden
        return EqualsBuilder.reflectionEquals(this, obj);
    }

    /**
     * Returns a hash code value for this object, based on internal values.
     *
     * @return  a hash code value for this object.
     */
    @Override
    public int hashCode() {                                     // Overridden
        return HashCodeBuilder.reflectionHashCode(this);
    }

    /**
     * Get the type of this extensibility element.
     *
     * @return  the type of this extensibility element.
     */
    public QName getElementType() {
        return elementType;
    }

    /**
     * Set the type of this extensibility element.
     *
     * @param  elementType  the type.
     */
    public void setElementType(QName elementType) {
        this.elementType = elementType;
    }

    /**
     * Get whether or not the semantics of this extension are required. Relates
     * to the <i>wsdl:required</i> attribute.
     *
     * @return  whether or not the semantics of this extension are required.
     */
    public Boolean getRequired() {
        return required;
    }

    /**
     * Set whether or not the semantics of this extension are required. Relates
     * to the <i>wsdl:required</i> attribute.
     *
     * @param  required  the required flag.
     */
    public void setRequired(Boolean required) {
        this.required = required;
    }

    /**
     * Returns the input copy Cobol. May be the same as the output copy Cobol or
     * it may be a different one.
     *
     * @return  the input copy Cobol.
     */
    public String getCopyCobol() {
        return copyCobol;
    }

    /**
     * Sets the input copy Cobol.
     *
     * @param  copyCobol  the new input copy Cobol to set.
     */
    public void setCopyCobol(String copyCobol) {
        this.copyCobol = copyCobol;
    }

    /**
     * Returns the service package name.
     *
     * @return  the service package name.
     */
    public String getServicePackageName() {
        return svcPackageName;
    }

    /**
     * Sets the service package name.
     *
     * @param  svcPackageName  the new service package name to set.
     */
    public void setServicePackageName(String svcPackageName) {
        this.svcPackageName = svcPackageName;
    }

    /**
     * Gets the code page.
     *
     * @return  the code page.
     */
    public String getCodePage() {
        return codePage;
    }

    /**
     * Sets the code page.
     *
     * @param  codePage  the new code page to set.
     */
    public void setCodePage(String codePage) {
        this.codePage = codePage;
    }

    /**
     * Gets the output copy Cobol. May be the same as the input copy Cobol or it
     * may be a different one.
     *
     * @return  the output copy Cobol.
     */
    public String getOutputCopyCobol() {
        return outputCopyCobol;
    }

    /**
     * Sets the output copy Cobol. May be the same as the input copy Cobol or it
     * may be a different one.
     *
     * @param  outputCopyCobol  the new output copy Cobol to set.
     */
    public void setOutputCopyCobol(String outputCopyCobol) {
        this.outputCopyCobol = outputCopyCobol;
    }

    /**
     * Gets the flag indicating if the input copy Cobol and the output copy
     * Cobol are the same.
     *
     * @return  <code>true</code> if the copy Cobol used for the input is used
     *          also for the output; <code>false</code> if the input copy Cobol
     *          differs from the output copy Cobol.
     * @see     #getCopyCobol()
     * @see     #getOutputCopyCobol()
     */
    public Boolean getSameCopyCobol() {
        return sameCopyCobol;
    }

    /**
     * Sets the flag indicating if the input copy Cobol and the output copy
     * Cobol are the same.
     *
     * @param  sameCopyCobol  the new flag value to set.
     * @see    #setCopyCobol(String)
     * @see    #setOutputCopyCobol(String)
     */
    public void setSameCopyCobol(Boolean sameCopyCobol) {
        this.sameCopyCobol = sameCopyCobol;
    }
}
