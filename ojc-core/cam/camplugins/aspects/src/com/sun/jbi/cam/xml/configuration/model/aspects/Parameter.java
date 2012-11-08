/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)Parameter.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**
 *
 */
package com.sun.jbi.cam.xml.configuration.model.aspects;

import java.io.File;
import java.io.Serializable;

import javax.xml.namespace.QName;

import com.sun.jbi.cam.common.EqualsUtil;
import com.sun.jbi.cam.common.HashCodeUtil;

/**
 * @author graj
 *
 */
public class Parameter implements Serializable {

    QName partnerLink;

    String roleName;

    QName portType;

    String operation;

    QName messageType;

    File file;

    boolean transformJBI = true;

    /**
     *
     */
    public Parameter() {
        // TODO Auto-generated constructor stub
    }

    /**
     * @param partnerLink
     * @param roleName
     * @param portType
     * @param operation
     * @param messageType
     * @param file
     * @param transformJBI
     */
    public Parameter(QName partnerLink, String roleName, QName portType,
            String operation, QName messageType, File file, boolean transformJBI) {
        super();
        this.partnerLink = partnerLink;
        this.roleName = roleName;
        this.portType = portType;
        this.operation = operation;
        this.messageType = messageType;
        this.file = file;
        this.transformJBI = transformJBI;
    }

    /**
     * @param partnerLinkString
     * @param roleName
     * @param portTypeString
     * @param operation
     * @param messageTypeString
     * @param fileNameString
     * @param transformJBIString
     */
    public Parameter(String partnerLinkString, String roleName,
            String portTypeString, String operation, String messageTypeString,
            String fileNameString, String transformJBIString) {
        super();
        this.partnerLink = new QName(partnerLinkString);
        this.roleName = roleName;
        this.portType = new QName(portTypeString);
        this.operation = operation;
        this.messageType = new QName(messageTypeString);
        this.file = new File(fileNameString);
        this.transformJBI = Boolean.getBoolean(transformJBIString);
    }

    /**
     * @param partnerLink
     * @param roleName
     * @param portType
     * @param operation
     * @param messageType
     * @param fileName
     * @param transformJBI
     */
    public Parameter(QName partnerLink, String roleName, QName portType,
            String operation, QName messageType, String fileName,
            boolean transformJBI) {
        super();
        this.partnerLink = partnerLink;
        this.roleName = roleName;
        this.portType = portType;
        this.operation = operation;
        this.messageType = messageType;
        this.file = new File(fileName);
        this.transformJBI = transformJBI;
    }

    /**
     * @return the transformJBI
     */
    public boolean isTransformJBI() {
        return transformJBI;
    }

    /**
     * @param transformJBI
     *            the transformJBI to set
     */
    public void setTrasformJBI(boolean transformJBI) {
        this.transformJBI = transformJBI;
    }

    /**
     * @return the file
     */
    public File getFile() {
        return file;
    }

    /**
     * @param file
     *            the file to set
     */
    public void setFile(File file) {
        this.file = file;
    }

    /**
     * @param fileName
     *            the file to set
     */
    public void setFile(String fileName) {
        this.file = new File(fileName);
    }

    /**
     * @return the messageType
     */
    public QName getMessageType() {
        return messageType;
    }

    /**
     * @param messageType
     *            the messageType to set
     */
    public void setMessageType(QName messageType) {
        this.messageType = messageType;
    }

    /**
     * @return the operation
     */
    public String getOperation() {
        return operation;
    }

    /**
     * @param operation
     *            the operation to set
     */
    public void setOperation(String operation) {
        this.operation = operation;
    }

    /**
     * @return the partnerLink
     */
    public QName getPartnerLink() {
        return partnerLink;
    }

    /**
     * @param partnerLink
     *            the partnerLink to set
     */
    public void setPartnerLink(QName partnerLink) {
        this.partnerLink = partnerLink;
    }

    /**
     * @return the portType
     */
    public QName getPortType() {
        return portType;
    }

    /**
     * @param portType
     *            the portType to set
     */
    public void setPortType(QName portType) {
        this.portType = portType;
    }

    /**
     * @return the roleName
     */
    public String getRoleName() {
        return roleName;
    }

    /**
     * @param roleName
     *            the roleName to set
     */
    public void setRoleName(String roleName) {
        this.roleName = roleName;
    }

    /**
     * Indicates whether some other object is "equal to" this one. The equals
     * method implements an equivalence relation on non-null object references:
     *
     * - It is reflexive: for any non-null reference value x, x.equals(x) should
     * return true.
     * - It is symmetric: for any non-null reference values x and
     * y, x.equals(y) should return true if and only if y.equals(x) returns
     * true.
     * - It is transitive: for any non-null reference values x, y, and z,
     * if x.equals(y) returns true and y.equals(z) returns true, then
     * x.equals(z) should return true.
     * - It is consistent: for any non-null reference values x and y, multiple invocations of x.equals(y)
     * consistently return true or consistently return false, provided no
     * information used in equals comparisons on the objects is modified.
     * - For  any non-null reference value x, x.equals(null) should return false.
     *
     * The equals method for class Object implements the most discriminating
     * possible equivalence relation on objects; that is, for any non-null
     * reference values x and y, this method returns true if and only if x and y
     * refer to the same object (x == y has the value true). Note that it is
     * generally necessary to override the hashCode method whenever this method
     * is overridden, so as to maintain the general contract for the hashCode
     * method, which states that equal objects must have equal hash codes.
     *
     * @param aThat -
     *            the reference object with which to compare.
     * @returns true if this object is the same as the obj argument; false
     *          otherwise.
     */
    public boolean equals(Object aThat) {
        // check for self-comparison
        if (this == aThat) {
            return true;
        }

        // use instanceof instead of getClass here for two reasons
        // 1. if need be, it can match any supertype, and not just one class;
        // 2. it renders an explict check for "that == null" redundant, since
        // it does the check for null already - "null instanceof [type]" always
        // returns false. (See Effective Java by Joshua Bloch.)
        if (!(aThat instanceof Parameter)) {
            return false;
        }
        // Alternative to the above line :
        // if ( aThat == null || aThat.getClass() != this.getClass() ) return
        // false;

        // cast to native object is now safe
        Parameter that = (Parameter) aThat;

        // now a proper field-by-field evaluation can be made
        return EqualsUtil.areEqual(this.transformJBI, that.transformJBI)
                && EqualsUtil.areEqual(this.file, that.file)
                && EqualsUtil.areEqual(this.messageType, that.messageType)
                && EqualsUtil.areEqual(this.operation, that.operation)
                && EqualsUtil.areEqual(this.portType, that.portType)
                && EqualsUtil.areEqual(this.partnerLink, that.partnerLink)
                && EqualsUtil.areEqual(this.roleName, that.roleName);
    }

    /**
     * Returns a hash code value for the object. This method is supported for the benefit of hashtables such as those provided by java.util.Hashtable.
     * The general contract of hashCode is:
     *     - Whenever it is invoked on the same object more than once during an execution of a Java application, the hashCode method must consistently return the same integer, provided no information used in equals comparisons on the object is modified. This integer need not remain consistent from one execution of an application to another execution of the same application.
     *     - If two objects are equal according to the equals(Object) method, then calling the hashCode method on each of the two objects must produce the same integer result.
     *     - It is not required that if two objects are unequal according to the equals(java.lang.Object) method, then calling the hashCode method on each of the two objects must produce distinct integer results. However, the programmer should be aware that producing distinct integer results for unequal objects may improve the performance of hashtables. 
     * As much as is reasonably practical, the hashCode method defined by class Object does return distinct integers for distinct objects. (This is typically implemented by converting the internal address of the object into an integer, but this implementation technique is not required by the JavaTM programming language.)
     * 
     * @returns a hash code value for this object.
     */
    public int hashCode() {
        //this style of lazy initialization is
        //suitable only if the object is immutable
        int fHashCode = 0;
        if (fHashCode == 0) {
            int result = HashCodeUtil.SEED;
            result = HashCodeUtil.hash(result, this.transformJBI);
            result = HashCodeUtil.hash(result, this.file);
            result = HashCodeUtil.hash(result, this.messageType);
            result = HashCodeUtil.hash(result, this.operation);
            result = HashCodeUtil.hash(result, this.portType);
            result = HashCodeUtil.hash(result, this.partnerLink);
            result = HashCodeUtil.hash(result, this.roleName);
            fHashCode = result;
        }
        return fHashCode;
    }

    public void dump() {
        System.out.println("// *****************************************");
        System.out.println("//       partnerLink = " + partnerLink);
        System.out.println("//       roleName = " + roleName);
        System.out.println("//       portType = " + portType);
        System.out.println("//       operation = " + operation);
        System.out.println("//       messageType = " + messageType);
        if (file != null) {
            System.out.println("//       file = " + file.getAbsolutePath());
        } else {
            System.out.println("//       file = " + file);
        }
        System.out.println("//       transformJBI = " + transformJBI);
        System.out.println("// *****************************************");
    }

    /**
     * @param args
     */
    public static void main(String[] args) {
        // TODO Auto-generated method stub

    }

}
