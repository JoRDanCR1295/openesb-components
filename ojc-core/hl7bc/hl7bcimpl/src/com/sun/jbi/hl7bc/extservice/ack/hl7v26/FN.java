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
import javax.xml.bind.annotation.XmlSeeAlso;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for FN complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="FN">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{urn:hl7-org:v2xml}FN.1" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}FN.2" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}FN.3" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}FN.4" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}FN.5" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "FN", propOrder = {
    "fn1",
    "fn2",
    "fn3",
    "fn4",
    "fn5"
})
@XmlSeeAlso({
    XCN2CONTENT.class,
    PPN2CONTENT.class,
    XPN1CONTENT.class
})
public class FN {

    @XmlElement(name = "FN.1")
    protected FN1CONTENT fn1;
    @XmlElement(name = "FN.2")
    protected FN2CONTENT fn2;
    @XmlElement(name = "FN.3")
    protected FN3CONTENT fn3;
    @XmlElement(name = "FN.4")
    protected FN4CONTENT fn4;
    @XmlElement(name = "FN.5")
    protected FN5CONTENT fn5;

    /**
     * Gets the value of the fn1 property.
     * 
     * @return
     *     possible object is
     *     {@link FN1CONTENT }
     *     
     */
    public FN1CONTENT getFN1() {
        return fn1;
    }

    /**
     * Sets the value of the fn1 property.
     * 
     * @param value
     *     allowed object is
     *     {@link FN1CONTENT }
     *     
     */
    public void setFN1(FN1CONTENT value) {
        this.fn1 = value;
    }

    /**
     * Gets the value of the fn2 property.
     * 
     * @return
     *     possible object is
     *     {@link FN2CONTENT }
     *     
     */
    public FN2CONTENT getFN2() {
        return fn2;
    }

    /**
     * Sets the value of the fn2 property.
     * 
     * @param value
     *     allowed object is
     *     {@link FN2CONTENT }
     *     
     */
    public void setFN2(FN2CONTENT value) {
        this.fn2 = value;
    }

    /**
     * Gets the value of the fn3 property.
     * 
     * @return
     *     possible object is
     *     {@link FN3CONTENT }
     *     
     */
    public FN3CONTENT getFN3() {
        return fn3;
    }

    /**
     * Sets the value of the fn3 property.
     * 
     * @param value
     *     allowed object is
     *     {@link FN3CONTENT }
     *     
     */
    public void setFN3(FN3CONTENT value) {
        this.fn3 = value;
    }

    /**
     * Gets the value of the fn4 property.
     * 
     * @return
     *     possible object is
     *     {@link FN4CONTENT }
     *     
     */
    public FN4CONTENT getFN4() {
        return fn4;
    }

    /**
     * Sets the value of the fn4 property.
     * 
     * @param value
     *     allowed object is
     *     {@link FN4CONTENT }
     *     
     */
    public void setFN4(FN4CONTENT value) {
        this.fn4 = value;
    }

    /**
     * Gets the value of the fn5 property.
     * 
     * @return
     *     possible object is
     *     {@link FN5CONTENT }
     *     
     */
    public FN5CONTENT getFN5() {
        return fn5;
    }

    /**
     * Sets the value of the fn5 property.
     * 
     * @param value
     *     allowed object is
     *     {@link FN5CONTENT }
     *     
     */
    public void setFN5(FN5CONTENT value) {
        this.fn5 = value;
    }

}
