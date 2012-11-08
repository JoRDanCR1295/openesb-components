/*
 * SwiftMessageBase.java
 *
 * Created on April 14, 2007, 12:17 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package com.sun.jbi.swiftbc.extensions.base;

import javax.xml.namespace.QName;

/**
 *
 * @author Sun Microsystems Inc.
 */
public class SwiftMessageBase implements javax.wsdl.extensions.ExtensibilityElement{
    private QName elementType;
    private Boolean required;
    private String encodingStyle;
    private String useType;
    /** Creates a new instance of SwiftMessageBase */
    public SwiftMessageBase() {
    }
    
    public void setElementType(QName elementType) {
        this.elementType = elementType;
    }
    
    public QName getElementType() {
        return elementType;
    }
    
    public void setRequired(Boolean required) {
        this.required = required;
    }
    
    public Boolean getRequired() {
        return required;
    }
    
   public void setUseType(String ut){
       useType = ut;
   } 
    public String getUseType() {
        return useType;
    }
    
    public String getEncodingStyle() {
        return encodingStyle;
    }
    
    public void setEncodingStyle(String es){
        encodingStyle = es;
    }
}
