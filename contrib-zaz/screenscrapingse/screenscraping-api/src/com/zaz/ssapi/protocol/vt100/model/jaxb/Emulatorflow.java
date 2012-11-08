//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, vhudson-jaxb-ri-2.1-463 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2008.09.09 at 03:09:11 PM CST 
//


package com.zaz.ssapi.protocol.vt100.model.jaxb;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for anonymous complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType>
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="host" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="port" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *         &lt;element name="screen" maxOccurs="unbounded">
 *           &lt;complexType>
 *             &lt;complexContent>
 *               &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *                 &lt;sequence>
 *                   &lt;element name="field">
 *                     &lt;complexType>
 *                       &lt;complexContent>
 *                         &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *                           &lt;sequence>
 *                             &lt;element name="value" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *                             &lt;element name="newvalue" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *                             &lt;element name="isparam" type="{http://www.w3.org/2001/XMLSchema}boolean"/>
 *                             &lt;element name="paraname" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *                             &lt;element name="comment" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *                           &lt;/sequence>
 *                         &lt;/restriction>
 *                       &lt;/complexContent>
 *                     &lt;/complexType>
 *                   &lt;/element>
 *                   &lt;element name="output" type="{http://www.w3.org/2001/XMLSchema}boolean"/>
 *                 &lt;/sequence>
 *               &lt;/restriction>
 *             &lt;/complexContent>
 *           &lt;/complexType>
 *         &lt;/element>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "", propOrder = {
    "host",
    "port",
    "screen"
})
@XmlRootElement(name = "emulatorflow")
public class Emulatorflow {

    @XmlElement(required = true)
    protected String host;
    protected int port;
    @XmlElement(required = true)
    protected List<Emulatorflow.Screen> screen;

    /**
     * Gets the value of the host property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getHost() {
        return host;
    }

    /**
     * Sets the value of the host property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setHost(String value) {
        this.host = value;
    }

    /**
     * Gets the value of the port property.
     * 
     */
    public int getPort() {
        return port;
    }

    /**
     * Sets the value of the port property.
     * 
     */
    public void setPort(int value) {
        this.port = value;
    }

    /**
     * Gets the value of the screen property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the screen property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getScreen().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link Emulatorflow.Screen }
     * 
     * 
     */
    public List<Emulatorflow.Screen> getScreen() {
        if (screen == null) {
            screen = new ArrayList<Emulatorflow.Screen>();
        }
        return this.screen;
    }


    /**
     * <p>Java class for anonymous complex type.
     * 
     * <p>The following schema fragment specifies the expected content contained within this class.
     * 
     * <pre>
     * &lt;complexType>
     *   &lt;complexContent>
     *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
     *       &lt;sequence>
     *         &lt;element name="field">
     *           &lt;complexType>
     *             &lt;complexContent>
     *               &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
     *                 &lt;sequence>
     *                   &lt;element name="value" type="{http://www.w3.org/2001/XMLSchema}string"/>
     *                   &lt;element name="newvalue" type="{http://www.w3.org/2001/XMLSchema}string"/>
     *                   &lt;element name="isparam" type="{http://www.w3.org/2001/XMLSchema}boolean"/>
     *                   &lt;element name="paraname" type="{http://www.w3.org/2001/XMLSchema}string"/>
     *                   &lt;element name="comment" type="{http://www.w3.org/2001/XMLSchema}string"/>
     *                 &lt;/sequence>
     *               &lt;/restriction>
     *             &lt;/complexContent>
     *           &lt;/complexType>
     *         &lt;/element>
     *         &lt;element name="output" type="{http://www.w3.org/2001/XMLSchema}boolean"/>
     *       &lt;/sequence>
     *     &lt;/restriction>
     *   &lt;/complexContent>
     * &lt;/complexType>
     * </pre>
     * 
     * 
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlType(name = "", propOrder = {
        "field",
        "output"
    })
    public static class Screen {

        @XmlElement(required = true)
        protected Emulatorflow.Screen.Field field;
        protected boolean output;

        /**
         * Gets the value of the field property.
         * 
         * @return
         *     possible object is
         *     {@link Emulatorflow.Screen.Field }
         *     
         */
        public Emulatorflow.Screen.Field getField() {
            return field;
        }

        /**
         * Sets the value of the field property.
         * 
         * @param value
         *     allowed object is
         *     {@link Emulatorflow.Screen.Field }
         *     
         */
        public void setField(Emulatorflow.Screen.Field value) {
            this.field = value;
        }

        /**
         * Gets the value of the output property.
         * 
         */
        public boolean isOutput() {
            return output;
        }

        /**
         * Sets the value of the output property.
         * 
         */
        public void setOutput(boolean value) {
            this.output = value;
        }


        /**
         * <p>Java class for anonymous complex type.
         * 
         * <p>The following schema fragment specifies the expected content contained within this class.
         * 
         * <pre>
         * &lt;complexType>
         *   &lt;complexContent>
         *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
         *       &lt;sequence>
         *         &lt;element name="value" type="{http://www.w3.org/2001/XMLSchema}string"/>
         *         &lt;element name="newvalue" type="{http://www.w3.org/2001/XMLSchema}string"/>
         *         &lt;element name="isparam" type="{http://www.w3.org/2001/XMLSchema}boolean"/>
         *         &lt;element name="paraname" type="{http://www.w3.org/2001/XMLSchema}string"/>
         *         &lt;element name="comment" type="{http://www.w3.org/2001/XMLSchema}string"/>
         *       &lt;/sequence>
         *     &lt;/restriction>
         *   &lt;/complexContent>
         * &lt;/complexType>
         * </pre>
         * 
         * 
         */
        @XmlAccessorType(XmlAccessType.FIELD)
        @XmlType(name = "", propOrder = {
            "value",
            "newvalue",
            "isparam",
            "paraname",
            "comment"
        })
        public static class Field {

            @XmlElement(required = true)
            protected String value;
            @XmlElement(required = true)
            protected String newvalue;
            protected boolean isparam;
            @XmlElement(required = true)
            protected String paraname;
            @XmlElement(required = true)
            protected String comment;

            /**
             * Gets the value of the value property.
             * 
             * @return
             *     possible object is
             *     {@link String }
             *     
             */
            public String getValue() {
                return value;
            }

            /**
             * Sets the value of the value property.
             * 
             * @param value
             *     allowed object is
             *     {@link String }
             *     
             */
            public void setValue(String value) {
                this.value = value;
            }

            /**
             * Gets the value of the newvalue property.
             * 
             * @return
             *     possible object is
             *     {@link String }
             *     
             */
            public String getNewvalue() {
                return newvalue;
            }

            /**
             * Sets the value of the newvalue property.
             * 
             * @param value
             *     allowed object is
             *     {@link String }
             *     
             */
            public void setNewvalue(String value) {
                this.newvalue = value;
            }

            /**
             * Gets the value of the isparam property.
             * 
             */
            public boolean isIsparam() {
                return isparam;
            }

            /**
             * Sets the value of the isparam property.
             * 
             */
            public void setIsparam(boolean value) {
                this.isparam = value;
            }

            /**
             * Gets the value of the paraname property.
             * 
             * @return
             *     possible object is
             *     {@link String }
             *     
             */
            public String getParaname() {
                return paraname;
            }

            /**
             * Sets the value of the paraname property.
             * 
             * @param value
             *     allowed object is
             *     {@link String }
             *     
             */
            public void setParaname(String value) {
                this.paraname = value;
            }

            /**
             * Gets the value of the comment property.
             * 
             * @return
             *     possible object is
             *     {@link String }
             *     
             */
            public String getComment() {
                return comment;
            }

            /**
             * Sets the value of the comment property.
             * 
             * @param value
             *     allowed object is
             *     {@link String }
             *     
             */
            public void setComment(String value) {
                this.comment = value;
            }

        }

    }

}
