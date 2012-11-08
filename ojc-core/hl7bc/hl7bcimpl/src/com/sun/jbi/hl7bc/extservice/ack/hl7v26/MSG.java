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
 * <p>Java class for MSG complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="MSG">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{urn:hl7-org:v2xml}MSG.1" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}MSG.2" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}MSG.3" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "MSG", propOrder = {
    "msg1",
    "msg2",
    "msg3"
})
@XmlSeeAlso({
    MSH9CONTENT.class
})
public class MSG {

    @XmlElement(name = "MSG.1")
    protected MSG1CONTENT msg1;
    @XmlElement(name = "MSG.2")
    protected MSG2CONTENT msg2;
    @XmlElement(name = "MSG.3")
    protected MSG3CONTENT msg3;

    /**
     * Gets the value of the msg1 property.
     * 
     * @return
     *     possible object is
     *     {@link MSG1CONTENT }
     *     
     */
    public MSG1CONTENT getMSG1() {
        return msg1;
    }

    /**
     * Sets the value of the msg1 property.
     * 
     * @param value
     *     allowed object is
     *     {@link MSG1CONTENT }
     *     
     */
    public void setMSG1(MSG1CONTENT value) {
        this.msg1 = value;
    }

    /**
     * Gets the value of the msg2 property.
     * 
     * @return
     *     possible object is
     *     {@link MSG2CONTENT }
     *     
     */
    public MSG2CONTENT getMSG2() {
        return msg2;
    }

    /**
     * Sets the value of the msg2 property.
     * 
     * @param value
     *     allowed object is
     *     {@link MSG2CONTENT }
     *     
     */
    public void setMSG2(MSG2CONTENT value) {
        this.msg2 = value;
    }

    /**
     * Gets the value of the msg3 property.
     * 
     * @return
     *     possible object is
     *     {@link MSG3CONTENT }
     *     
     */
    public MSG3CONTENT getMSG3() {
        return msg3;
    }

    /**
     * Sets the value of the msg3 property.
     * 
     * @param value
     *     allowed object is
     *     {@link MSG3CONTENT }
     *     
     */
    public void setMSG3(MSG3CONTENT value) {
        this.msg3 = value;
    }

}
