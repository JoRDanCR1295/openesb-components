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
 * <p>Java class for SCV complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="SCV">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{urn:hl7-org:v2xml}SCV.1" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}SCV.2" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "SCV", propOrder = {
    "scv1",
    "scv2"
})
public class SCV {

    @XmlElement(name = "SCV.1")
    protected SCV1CONTENT scv1;
    @XmlElement(name = "SCV.2")
    protected SCV2CONTENT scv2;

    /**
     * Gets the value of the scv1 property.
     * 
     * @return
     *     possible object is
     *     {@link SCV1CONTENT }
     *     
     */
    public SCV1CONTENT getSCV1() {
        return scv1;
    }

    /**
     * Sets the value of the scv1 property.
     * 
     * @param value
     *     allowed object is
     *     {@link SCV1CONTENT }
     *     
     */
    public void setSCV1(SCV1CONTENT value) {
        this.scv1 = value;
    }

    /**
     * Gets the value of the scv2 property.
     * 
     * @return
     *     possible object is
     *     {@link SCV2CONTENT }
     *     
     */
    public SCV2CONTENT getSCV2() {
        return scv2;
    }

    /**
     * Sets the value of the scv2 property.
     * 
     * @param value
     *     allowed object is
     *     {@link SCV2CONTENT }
     *     
     */
    public void setSCV2(SCV2CONTENT value) {
        this.scv2 = value;
    }

}