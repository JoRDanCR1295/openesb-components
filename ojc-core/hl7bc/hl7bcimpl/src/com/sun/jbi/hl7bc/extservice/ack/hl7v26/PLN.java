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
 * <p>Java class for PLN complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="PLN">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{urn:hl7-org:v2xml}PLN.1" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}PLN.2" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}PLN.3" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}PLN.4" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "PLN", propOrder = {
    "pln1",
    "pln2",
    "pln3",
    "pln4"
})
public class PLN {

    @XmlElement(name = "PLN.1")
    protected PLN1CONTENT pln1;
    @XmlElement(name = "PLN.2")
    protected PLN2CONTENT pln2;
    @XmlElement(name = "PLN.3")
    protected PLN3CONTENT pln3;
    @XmlElement(name = "PLN.4")
    protected PLN4CONTENT pln4;

    /**
     * Gets the value of the pln1 property.
     * 
     * @return
     *     possible object is
     *     {@link PLN1CONTENT }
     *     
     */
    public PLN1CONTENT getPLN1() {
        return pln1;
    }

    /**
     * Sets the value of the pln1 property.
     * 
     * @param value
     *     allowed object is
     *     {@link PLN1CONTENT }
     *     
     */
    public void setPLN1(PLN1CONTENT value) {
        this.pln1 = value;
    }

    /**
     * Gets the value of the pln2 property.
     * 
     * @return
     *     possible object is
     *     {@link PLN2CONTENT }
     *     
     */
    public PLN2CONTENT getPLN2() {
        return pln2;
    }

    /**
     * Sets the value of the pln2 property.
     * 
     * @param value
     *     allowed object is
     *     {@link PLN2CONTENT }
     *     
     */
    public void setPLN2(PLN2CONTENT value) {
        this.pln2 = value;
    }

    /**
     * Gets the value of the pln3 property.
     * 
     * @return
     *     possible object is
     *     {@link PLN3CONTENT }
     *     
     */
    public PLN3CONTENT getPLN3() {
        return pln3;
    }

    /**
     * Sets the value of the pln3 property.
     * 
     * @param value
     *     allowed object is
     *     {@link PLN3CONTENT }
     *     
     */
    public void setPLN3(PLN3CONTENT value) {
        this.pln3 = value;
    }

    /**
     * Gets the value of the pln4 property.
     * 
     * @return
     *     possible object is
     *     {@link PLN4CONTENT }
     *     
     */
    public PLN4CONTENT getPLN4() {
        return pln4;
    }

    /**
     * Sets the value of the pln4 property.
     * 
     * @param value
     *     allowed object is
     *     {@link PLN4CONTENT }
     *     
     */
    public void setPLN4(PLN4CONTENT value) {
        this.pln4 = value;
    }

}