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
 * @(#)DOMJCOTransformer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.sapbc;

import com.sap.mw.jco.JCO;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.sapbc.util.ApproximateName;
import com.sun.org.apache.xml.internal.serialize.OutputFormat;
import com.sun.org.apache.xml.internal.serialize.XMLSerializer;
import com.sun.wsdl.model.WSDLDefinitions;
import java.io.StringWriter;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.namespace.QName;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.ls.DOMImplementationLS;
import org.w3c.dom.ls.LSSerializer;

/**
 * Transforms XML data into a SAP JCO Request.
 *
 * @author Noel Ang (noel.ang@sun.com)
 */
public class DOMJCOTransformer {
    
    public DOMJCOTransformer(WSDLDefinitions def) {
        mDefinition = def;
    }
    
    public void transform(JCO.Request request, Node node) {
        final Node startStart = node;
        //nodetostring(node);        
        
        if (node.getNodeType() != Node.ELEMENT_NODE) {
            throw new IllegalArgumentException(
                    mMessages.getString(
                    "DOMJCOTransformer.Start_node_not_element",
                    node.getClass().getName()));
        }
        
        Node child = node.getFirstChild();
        //nodetostring(child);        
        while (child != null) {
            if (child.getNodeType() == Node.ELEMENT_NODE) {
                processElement(child, request);
            }
            child = child.getNextSibling();
        }
    }

    private void processElement(Node node, JCO.Record record) {
        if (node.getNodeType() == Node.ELEMENT_NODE) {

            if (isForJcoValue(node)) {
                String value = node.getTextContent();
                ApproximateName name = new ApproximateName(node.getNodeName());
                setField(record, name, value);
            }


            else if (isForJcoTable(node)) {
                ApproximateName name = new ApproximateName(node.getNodeName());
                processTable(node, findTable(record, name));
            }

            else if (isForJcoStructure(node)) {
                ApproximateName name = new ApproximateName(node.getNodeName());
                JCO.Record structure = findStructure(record, name);
                Node child = node.getFirstChild();
                while (child != null) {
                    processElement(child, structure);
                    child = child.getNextSibling();
                }
            }
            
            else {
                mLogger.log(Level.WARNING,
                        "DOMJCOTransformer.Unhandleable_node",
                        node.getNodeName());
            }
        }
    }
    
    private void processTable(Node node, JCO.Table table) {
        Node child = node.getFirstChild();
        while (child != null) {
            if (child.getNodeType() == Node.ELEMENT_NODE) {
                if (JCOTABLE_ROW_NAME.equals(child.getLocalName())) {
                    processTableRow(child, table);
                } else {
                    mLogger.log(Level.WARNING,
                            "DOMJCOTransformer.Unhandleable_node",
                            child.getNodeName());
                }
            }
            child = child.getNextSibling();
        }
    }

    private void processTableRow(Node node, JCO.Table table) {
        Node child = node.getFirstChild();
        if (child == null) {
            return;
        }
        
        table.appendRow();
        
        while (child != null) {
            
            if (child.getNodeType() == Node.ELEMENT_NODE) {
                
                if (isForJcoValue(child)) {
                    String value = child.getTextContent();
                    ApproximateName name = new ApproximateName(child.getNodeName());
                    setField(table, name, value);
                }
                
                else if (isForJcoStructure(child)) {
                    processElement(child, table);
                }
                
                else if (isForJcoTable(child)) {
                    ApproximateName name = new ApproximateName(child.getNodeName());
                    processTable(child, findTable(table, name));
                }
                
                else {
                    mLogger.log(Level.WARNING,
                            "DOMJCOTransformer.Unhandleable_node",
                            child.getNodeName());
                }
            }
            child = child.getNextSibling();
        }
    }
    
    /**
     * Decides if a given node represents a value that is usable in a
     * populating a field in a JCO.ParameterList.
     */
    private boolean isForJcoValue(Node node) {
        boolean isForValue = false;
        
        // A node is compatible if it is an element with CDATA and nothing else.
        if (node.hasChildNodes()) {
            if (node.getChildNodes().getLength() == 1) {
                Node innerChild = node.getFirstChild();
                isForValue = innerChild.getNodeType() == Node.TEXT_NODE;
            }
        }
        
        return isForValue;
    }

    /**
     * Decides if a given node represents a structure whose values are usable
     * in populating a JCO.Table.
     */
    private boolean isForJcoTable(Node node) {
        boolean isForTable = false;
        
        // For a message structure to qualify as JCO Table data, the following
        // criteria must be met:
        //
        // 1. The node must be an element.
        // 2. The node must have one or more child elements, named 'item'.
        // 3. Each of the 'item' elements must have at least one child element.
        if (node.getNodeType() == Node.ELEMENT_NODE) {
            Node child = node.getFirstChild();
            while (child != null) {
                String childName = child.getLocalName();
                boolean childHasNodes = child.hasChildNodes();
                child = child.getNextSibling();
                if (childName == null) continue;
                
                isForTable = JCOTABLE_ROW_NAME.equals(childName) && childHasNodes;
                if (!isForTable) {
                    break;
                }
            };
        }
        
        return isForTable;
    }
    
    /**
     * Decides if a given node represents a structure whose values are usable
     * in populating a JCO.Table.
     */
    private boolean isForJcoStructure(Node node) {
        boolean isForStructure = false;
        
        // For a message structure to qualify as JCO Structure data, the following
        // criteria must be met:
        //
        // 1. The node is not the root of a JCO table structure.
        // 2. The node must be an element.
        // 3. The node must have one or more child element.
        isForStructure = !isForJcoTable(node)
                && node.getNodeType() == Node.ELEMENT_NODE
                && node.hasChildNodes();
        
        return isForStructure;
    }
    
    private void setField(JCO.Record record, ApproximateName name, String value) {
        boolean success = false;
        boolean foundStructureOrTable = false;
        JCO.FieldIterator iter = record.fields();
        
        while (!success && iter.hasMoreFields()) {
            JCO.Field field = iter.nextField();
            String fieldName = field.getName();
            if (name.similar(fieldName)) {
                if (!field.isStructure() && !field.isTable()) {
                    mLogger.log(Level.FINEST,"DOMJCOTransformer:setField() setting ["+value+"] for field ["+fieldName+"]");
                    field.setValue(value);
                    success = true;
                } else {
                    foundStructureOrTable = true;
                }
            }
        }

        if (!success) {
            String msg = (foundStructureOrTable
                    ? mMessages.getString("DOMJCOTransformer.Unresolved_field_name_got_incompat_obj", name.toString())
                    : mMessages.getString("DOMJCOTransformer.Unresolved_field_name", name.toString()));
            mLogger.log(Level.SEVERE, msg);
            throw new RuntimeException(msg);
        }
    }
    
    private JCO.Table findTable(JCO.Record record, ApproximateName name) {
        boolean foundElementOrStructure = false;
        JCO.FieldIterator iter = record.fields();
        JCO.Table table = null;
        
        while (table == null && iter.hasMoreFields()) {
            JCO.Field field = iter.nextField();
            String fieldName = field.getName();
            if (name.similar(fieldName)) {
                if (field.isTable()) {
                    table = record.getTable(fieldName);
                } else {
                    foundElementOrStructure = true;
                }
            }
        }

        if (table == null) {
            String msg = (foundElementOrStructure
                    ? mMessages.getString("DOMJCOTransformer.Unresolved_table_got_incompat_obj", name.toString())
                    : mMessages.getString("DOMJCOTransformer.Unresolved_table", name.toString()));
            mLogger.log(Level.SEVERE, msg);
            throw new RuntimeException(msg);
        }
        
        return table;
    }
    
    private JCO.Structure findStructure(JCO.Record record, ApproximateName name) {
        boolean foundElementOrTable = false;
        JCO.FieldIterator iter = record.fields();
        JCO.Structure structure = null;
        
        while (structure == null && iter.hasMoreFields()) {
            JCO.Field field = iter.nextField();
            String fieldName = field.getName();
            if (name.similar(fieldName)) {
                if (field.isStructure()) {
                    structure = record.getStructure(fieldName);
                } else {
                    foundElementOrTable = true;
                }
            }
        }

        if (structure == null) {
            String msg = (foundElementOrTable
                    ? mMessages.getString("DOMJCOTransformer.Unresolved_struct_got_incompat_obj", name.toString())
                    : mMessages.getString("DOMJCOTransformer.Unresolved_struct", name.toString()));
            mLogger.log(Level.SEVERE, msg);
            throw new RuntimeException(msg);
        }
        
        return structure;
    }
    
    private QName qnameOf(final Node elem, final String defaultvalue) {
        String ns = elem.getNamespaceURI();
        
        if (ns != null) {
            return QName.valueOf("{" + ns + "}" + elem.getLocalName());
        }
        
        Node parent = elem.getParentNode();
        while (ns == null && parent != null) {
            ns = parent.getNamespaceURI();
        }
        
        if (ns == null) {
            String prefix = elem.getPrefix();
            if (prefix != null) {
                ns = (String) mDefinition.getNamespaces().get(prefix);
            }
        }
        
        if (ns == null) {
            ns = defaultvalue;
        }
        
        if (ns == null) {
            return null;
        } else {
            return QName.valueOf("{" + ns + "}" + elem.getLocalName());
        }
    }
    
    private final Logger mLogger = mMessages.getLogger(getClass());
    private final WSDLDefinitions mDefinition;
    
    private static final Messages mMessages = Messages.getMessages(DOMJCOTransformer.class);
    private static final String JCOTABLE_ROW_NAME = "item";
}
