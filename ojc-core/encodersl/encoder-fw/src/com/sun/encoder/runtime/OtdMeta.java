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
 * @(#)OtdMeta.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.runtime;

/**
 * The OTD run-time metadata interface describes the read-only metadata for a
 * single OTD node. This interface is implemented by an OTD to provide generic
 * access facilities, other than through named set/get methods. An OTD
 * provides a run-time tree of data nodes, following a static grammar (tree
 * description) defined at eGate configuration time. Each node is implemented
 * by some class that provides i.a. set/get methods to access the children of
 * an instance of that node. This implementation class also provides static
 * access to an OtdMeta object that describes the set of children, i.e. a
 * subset of the metadata. Rationale: It would be possible to include most of
 * this in the generated classes as static methods of the implenentation
 * classes, but that would make it hard to retrieve metadata on leaf nodes,
 * and would preclude an implementation sharing classes between nodes. By
 * contract, all implementations of this interface should be thread-safe. Note
 * that the metadata tree presented by this interface corresponds to an OTD
 * with all template/module references resolved. Because an OTD may contain
 * mututally recursive templates, the tree expansion could be infinite; hence,
 * we cannot provide something like getParent(), as there may be multiple
 * references to a node. A template reference is resolved in this view to a
 * composite of the original reference node and the root of the template, the
 * list of children immediately below the root is shared between all resolved
 * references.
 *
 * @author Michael Libourel
 * @version 
 */
public interface OtdMeta {
    /**
     * Tests whether this is a leaf node, not further described by the grammar.
     * The actual Java type of a leaf node may be a complex object, the leaf
     * node is just the boundary of the visibility of the interior complexity
     * of the data as far as the OTD metadata is concerned.
     *
     * @return true if leaf, else false
     */
    boolean isLeaf();

    /**
     * Tests whether this is a choice node.  If so, it cannot also be a leaf
     * node, and its class will support OtdNode.choice().
     *
     * @return true if leaf, else false
     */
    boolean isChoice ();

    /**
     * Contains the number of children in the grammar.
     *
     * @return child count, or -1 for leaf node
     */
    int getChildCount();

    /**
     * Gets a given child in the grammar. The index must be in range
     * 0...childCount-1.
     *
     * @param index the child index
     * @return the child
     * @throws IndexOutOfBoundsException for invalid index
     */
    OtdMeta getChild(int index)
        throws IndexOutOfBoundsException;

    /**
     * Returns the node describing the interal reference. This method should
     * only be called when getType() returns INREF. NOTE: NOT SURE ABOUT THIS
     * YET!!!
     *
     * @return the referred node
     * @throws NoSuchFieldException when not an internal reference
     */
    OtdMeta getReference()
        throws NoSuchFieldException;

    /**
     * Returns the display name of this node. This name is normally close to
     * the original metadata name (if any), and is the name normally seen by
     * the user. It may contain basically any Unicode text.
     *
     * @return the display name, or null if none
     */
    String getName();

    /**
     * Returns the child index of this node below its parent.
     *
     * @return a non-negative value, or -1 for the root node
     */
    int getIndex();

    /**
     * Returns the accessor basename of this node. This is a valid Java
     * identifier, with some additional constraints, unique below its parent
     * node, from which accessor method names are constructed using simple
     * prefixes.
     *
     * @return the display name, or null if none
     */
    String getJavaName();

    /**
     * Retrieves the list of standard access methods available. The result is
     * the OR'ed set of bits described by the constants in the local "Access"
     * interface. For example, if this set includes the bit Access.COUNT then
     * the implementation class for this node will have a "countXXX()" method,
     * where "XXX" is the node's accessor name.
     *
     * @return set of available access methods (e.g. Access.GET + Access.SET)
     */
    int getAccess();

    /**
     * Retrieves the node type used in the direct accessor methods. The value
     * must be one from the list in the local "Type" interface. For example,
     * if getType() == Type.SHORT then the implementation class will have an
     * accessor method "void setXxx(short value)", where "Xxx" is the accessor
     * basename, assuming the node is non-repeating and has read access.
     * Assertion: isLeaf() == (getType() != NODE).
     *
     * @return the node access type, e.g. Type.FLOAT
     */
    byte getType();

    /**
     * Tests whether this child is indexed below its parent.
     *
     * @return true if repeated, else false
     */
    boolean indexed();

    /**
     * Retrieves the class implementing the node type. For nodes with a
     * primitive type (e.g. OtdMeta.Type.BOOL), this will return the boxed
     * equivalent (i.e. java.lang.Boolean). Note: We'd like to call this
     * method "getClass", but that is taken...
     *
     * @return the (boxed) class
     */
    Class getNodeClass();

    /**
     * The generic interface for metadata extensions.
     *
     * @param name a property name
     *
     * @return the value, or null if unknown
     */
    Object getProperty(String name);

    /**
     * Mask values for the return value of "getAccess".
     */
    public static interface Access {
        /**
         * Flag: parent has "void addXxx()" accessor (repeated child).
         */
        public static final int ADD = 0x01;

        /**
         * Flag: parent has "int countXxx()" accessor.
         */
        public static final int COUNT = 0x02;

        /**
         * Flag: parent has "T getXxx()" or "T getXxx(int)" accessor.
         */
        public static final int GET = 0x04;

        /**
         * Flag: parent has "T letXxx()" or "T letXxx(int)" accessor.
         */
        public static final int LET = 0x08;

        /**
         * Flag: parent has "T[] getXxx()" accessor (repeated child).
         */
        public static final int GETALL = 0x10;

        /**
         * Flag: parent has "hasXxx()" accessor (optional child).
         */
        public static final int HAS = 0x20;

        /**
         * Flag: parent has "void setXxx(T)" or "void setxxx(int, T)" accessor.
         */
        public static final int SET = 0x40;

        /**
         * Flag: parent has "setXxx(T[])" accessor (repeated child).
         */
        public static final int SETALL = 0x80;
    }

    /**
     * Values to indicate the node type.
     */
    public static interface Type {
        // Primitive types.

        /**
         * Java type "boolean".
         */
        public static final byte BOOL = 1;

        /**
         * Java type "byte".
         */
        public static final byte BYTE = 2;

        /**
         * Java type "char".
         */
        public static final byte CHAR = 3;

        /**
         * Java type "double".
         */
        public static final byte DOUBLE = 4;

        /**
         * Java type "float".
         */
        public static final byte FLOAT = 5;

        /**
         * Java type "int".
         */
        public static final byte INT = 6;

        /**
         * Java type "long".
         */
        public static final byte LONG = 7;

        /**
         * Java type "short".
         */
        public static final byte SHORT = 8;

        // Civilized types.

        /**
         * Internal reference to node.
         */
        public static final byte INREF = 10;

        /**
         * Not a leaf node.
         */
        public static final byte NODE = 11;

        /**
         * Any non-primitive leaf node.
         */
        public static final byte OBJECT = 12;

        /**
         * Reference to an external top tree.
         */
        public static final byte EXREF = 14;

        /**
         * Reference to external embedded parsing.
         */
        public static final byte EPREF = 15;
    }

    /**
     * Values for commonly used property names.
     */
    public static interface Property {
        /**
         * Property name indicating if the bean supports container (Has an
         * extra node that represents the document)
         */
        public static final String SUPPORT_CONTAINER = "support_container";
        
        /**
         * Property name indicating if in NodePointer, name should be used
         * instead of Java name.
         */
        public static final String XPATH_USE_NAME = "xpath_use_name";
    }
}
