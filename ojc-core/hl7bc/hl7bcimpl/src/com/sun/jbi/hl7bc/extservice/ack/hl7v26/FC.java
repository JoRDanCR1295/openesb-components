//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.1-b02-fcs 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2008.12.04 at 12:45:30 AM IST 
//


package com.sun.jbi.hl7bc.extservice.ack.hl7v26;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for FC complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="FC">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{urn:hl7-org:v2xml}FC.1" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}FC.2" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "FC", propOrder = {
    "fc1",
    "fc2"
})
public class FC {

    @XmlElement(name = "FC.1")
    protected FC1CONTENT fc1;
    @XmlElement(name = "FC.2")
    protected FC2CONTENT fc2;

    /**
     * Gets the value of the fc1 property.
     * 
     * @return
     *     possible object is
     *     {@link FC1CONTENT }
     *     
     */
    public FC1CONTENT getFC1() {
        return fc1;
    }

    /**
     * Sets the value of the fc1 property.
     * 
     * @param value
     *     allowed object is
     *     {@link FC1CONTENT }
     *     
     */
    public void setFC1(FC1CONTENT value) {
        this.fc1 = value;
    }

    /**
     * Gets the value of the fc2 property.
     * 
     * @return
     *     possible object is
     *     {@link FC2CONTENT }
     *     
     */
    public FC2CONTENT getFC2() {
        return fc2;
    }

    /**
     * Sets the value of the fc2 property.
     * 
     * @param value
     *     allowed object is
     *     {@link FC2CONTENT }
     *     
     */
    public void setFC2(FC2CONTENT value) {
        this.fc2 = value;
    }

}
