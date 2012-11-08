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
 * <p>Java class for RCD complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="RCD">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{urn:hl7-org:v2xml}RCD.1" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}RCD.2" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}RCD.3" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "RCD", propOrder = {
    "rcd1",
    "rcd2",
    "rcd3"
})
public class RCD {

    @XmlElement(name = "RCD.1")
    protected RCD1CONTENT rcd1;
    @XmlElement(name = "RCD.2")
    protected RCD2CONTENT rcd2;
    @XmlElement(name = "RCD.3")
    protected RCD3CONTENT rcd3;

    /**
     * Gets the value of the rcd1 property.
     * 
     * @return
     *     possible object is
     *     {@link RCD1CONTENT }
     *     
     */
    public RCD1CONTENT getRCD1() {
        return rcd1;
    }

    /**
     * Sets the value of the rcd1 property.
     * 
     * @param value
     *     allowed object is
     *     {@link RCD1CONTENT }
     *     
     */
    public void setRCD1(RCD1CONTENT value) {
        this.rcd1 = value;
    }

    /**
     * Gets the value of the rcd2 property.
     * 
     * @return
     *     possible object is
     *     {@link RCD2CONTENT }
     *     
     */
    public RCD2CONTENT getRCD2() {
        return rcd2;
    }

    /**
     * Sets the value of the rcd2 property.
     * 
     * @param value
     *     allowed object is
     *     {@link RCD2CONTENT }
     *     
     */
    public void setRCD2(RCD2CONTENT value) {
        this.rcd2 = value;
    }

    /**
     * Gets the value of the rcd3 property.
     * 
     * @return
     *     possible object is
     *     {@link RCD3CONTENT }
     *     
     */
    public RCD3CONTENT getRCD3() {
        return rcd3;
    }

    /**
     * Sets the value of the rcd3 property.
     * 
     * @param value
     *     allowed object is
     *     {@link RCD3CONTENT }
     *     
     */
    public void setRCD3(RCD3CONTENT value) {
        this.rcd3 = value;
    }

}