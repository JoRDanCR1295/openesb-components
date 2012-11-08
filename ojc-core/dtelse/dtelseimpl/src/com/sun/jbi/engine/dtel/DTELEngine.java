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
 * @(#)DTELEngine.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.dtel;

import java.io.File;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.dom.DOMSource;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 *
 * @author jwaldorf
 */
public class DTELEngine {
    private static final String OUT_ROOT = "dtelOperation";
    private static final String OUT_MESSAGE = "output";
    private static final String OUT_ELEMENT = "outElement";

    private Document mDecisionTableDocument;
    private Element mDecisionTableElement;
    private String[] mFieldNames;
    private String[] mFieldTypes;
    private Field[] mParent;
    private LinkedList mRoot;
    private String mNamespace;

    public class Field {
        private String mName;
        private String mExpression;
        private String mType; // Test || Assignment
        private String mExpressionType; // ExactNumber || LessThanNumber || GreaterThanNumber || NumberRange || String
        private double mValue;
        private double mValueOne;
        private double mValueTwo;
        private Date mBeginDate;
        private Date mEndDate;
        private Date mDate;
        private String mExpressionOne;
        private String mExpressionTwo;
        private LinkedList mSubFields;
        private HashMap mAssignments;
        
        public Field() {
            mSubFields = new LinkedList();
            mAssignments = new HashMap();
        }
        
        public String getName() {
            return mName;
        }
        
        public void setName(String name) {
            mName = name;
        }
        
        public String getExpression() {
            return mExpression;
        }
        
        public void setExpression(String expression) {
            mExpression = expression;
        }
        
        public String getExpressionOne() {
            return mExpressionOne;
        }
        
        public void setExpressionOne(String expression) {
            mExpressionOne = expression;
        }
        
        public String getExpressionTwo() {
            return mExpressionTwo;
        }
        
        public void setExpressionTwo(String expression) {
            mExpressionTwo = expression;
        }
        
        public String getType() {
            return mType;
        }
        
        public void setType(String type) {
            mType = type;
        }
        
        public String getExpressionType() {
            return mExpressionType;
        }
        
        public void setExpressionType(String expressionType) {
            mExpressionType = expressionType;
        }
        
        public LinkedList getSubFields() {
            return mSubFields;
        }
        
        public void addSubField(Field field) {
            mSubFields.add(field);
        }
        
        public String addAssignmentRule(String name, String value) {
            mAssignments.put(name, value);
            return "string";
        }
        
        public HashMap getAssignmentRules() {
            return mAssignments;
        }
        
        public void parseData(String data) {
            // Check for {ExactNumber}
            Pattern p = Pattern.compile("[ ]*[0123456789]+[ ]*");
            Matcher m = p.matcher(data);
            if (m.matches()) {
                try {
                    String s[] = data.split("[ ]+");
                    mExpression = s[0];
                    mExpressionType = "ExactNumber";
                    mValue = Double.parseDouble(s[0]);
                    mType = "double";
                    return;
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
            
            // Check for {LessThanNumber}
            p = Pattern.compile("less[ ]+than[ ]+[0123456789]+[ ]*");
            m = p.matcher(data);
            if (m.matches()) {
                try {
                    String s[] = data.split("[ ]+");
                    mExpression = s[2];
                    mExpressionType = "LessThanNumber";
                    mValue = Double.parseDouble(s[2]);
                    mType = "double";
                    return;
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
            
            // Check for {GreaterThanNumber}
            p = Pattern.compile("greater[ ]+than[ ]+[0123456789]+[ ]*");
            m = p.matcher(data);
            if (m.matches()) {
                try {
                    String s[] = data.split("[ ]+");
                    mExpression = s[2];
                    mExpressionType = "GreaterThanNumber";
                    mValue = Double.parseDouble(s[2]);
                    mType = "double";
                    return;
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
            
            // Check for {NumberRange}
            p = Pattern.compile("[ ]*[0123456789]+[ ]*\\.\\.[ ]*[0123456789]+[ ]*");
            m = p.matcher(data);
            if (m.matches()) {
                try {
                    String s[] = data.split("[ ]+");
                    mExpressionOne = s[0];
                    mExpressionTwo = s[2];
                    mExpressionType = "NumberRange";
                    mValueOne = Double.parseDouble(s[0]);
                    mValueTwo = Double.parseDouble(s[2]);
                    mType = "double";
                    return;
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
            
            // Check for {DateRange}
            p = Pattern.compile("[ ]*[^ ]+[ ]*\\.\\.[ ]*[^ ]+[ ]*");
            m = p.matcher(data);
            if (m.matches()) {
                try {
                    String s[] = data.split("\\.\\.");
                    try {
                        DateFormat formatter = new SimpleDateFormat("MM/dd/yyyy");
                        mBeginDate = formatter.parse(s[0]);
                        mEndDate = formatter.parse(s[1]);
                        mExpressionType = "DateRange";
                        mType = "dateTime";
                        return;
                    } catch (Exception e) {
                        // Just continue if we cannot parse the dates.
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
            
            // Check for {BeforeDate}
            p = Pattern.compile("[ ]*before[ ]*[^ ]+[ ]*");
            m = p.matcher(data);
            if (m.matches()) {
                try {
                    String s[] = data.split("[ ]+");
                    try {
                        DateFormat formatter = new SimpleDateFormat("MM/dd/yyyy");
                        mDate = formatter.parse(s[1]);
                        mExpressionType = "BeforeDate";
                        mType = "dateTime";
                        return;
                    } catch (Exception e) {
                        // Just continue if we cannot parse the dates.
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
            
            // Check for {AfterDate}
            p = Pattern.compile("[ ]*after[ ]*[^ ]+[ ]*");
            m = p.matcher(data);
            if (m.matches()) {
                try {
                    String s[] = data.split("[ ]+");
                    try {
                        DateFormat formatter = new SimpleDateFormat("MM/dd/yyyy");
                        mDate = formatter.parse(s[1]);
                        mExpressionType = "AfterDate";
                        mType = "dateTime";
                        return;
                    } catch (Exception e) {
                        // Just continue if we cannot parse the dates.
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
            
            try {
//                DateFormat formatter = new SimpleDateFormat("MM/dd/yyyy");
                mDate = DateFormat.getDateTimeInstance().parse(data);
                mExpressionType = "ExactDate";
                mType = "dateTime";
                return;
            } catch (Exception e) {
                // If we cannot parse the date then it must be a string
            }
            
            mExpressionType = "ExactString";
            mExpression = data;
            mType = "string";
        }
        
        public boolean eval(String data) {
            if (mExpressionType.equals("ExactNumber")) {
                double d = Double.parseDouble(data);
                return d == mValue;
            } else if (mExpressionType.equals("LessThanNumber")) {
                double d = Double.parseDouble(data);
                return d < mValue;
            } else if (mExpressionType.equals("GreaterThanNumber")) {
                double d = Double.parseDouble(data);
                return d > mValue;
            } else if (mExpressionType.equals("NumberRange")) {
                double d = Double.parseDouble(data);
                return d >= mValueOne && d <= mValueTwo;
            } else if (mExpressionType.equals("BeforeDate")) {
                try {
                    DateFormat formatter = new SimpleDateFormat("MM/dd/yyyy");
                    Date date = formatter.parse(data);
                    return date.compareTo(mDate) < 0;
                } catch (Exception e) {
                    return false;
                }
            } else if (mExpressionType.equals("AfterDate")) {
                try {
                    DateFormat formatter = new SimpleDateFormat("MM/dd/yyyy");
                    Date date = formatter.parse(data);
                    return date.compareTo(mDate) > 0;
                } catch (Exception e) {
                    return false;
                }
            } else if (mExpressionType.equals("DateRange")) {
                try {
                    DateFormat formatter = new SimpleDateFormat("MM/dd/yyyy");
                    Date date = formatter.parse(data);
                    return date.compareTo(mBeginDate) >= 0 && date.compareTo(mEndDate) <= 0;
                } catch (Exception e) {
                    return false;
                }
            } else if (mExpressionType.equals("ExactDate")) {
                try {
                    DateFormat formatter = new SimpleDateFormat("MM/dd/yyyy");
                    Date date = formatter.parse(data);
                    return date.compareTo(mDate) == 0;
                } catch (Exception e) {
                    return false;
                }
            } else if (mExpressionType.equals("ExactString")) {
                if (mExpression.equals("*")) {
                    return true;
                }
                return data.equals(mExpression);
            } else {
                return false;
            }
        }
        
        public void assign(Document doc) {
            Set set = getAssignmentRules().entrySet();

            Element outputRoot = doc.getDocumentElement();

            for (Iterator i = set.iterator(); i.hasNext();) {
                Map.Entry me = (Map.Entry) i.next();
                String outputName = (String) me.getKey();
                String outputValue = (String) me.getValue();

                NodeList nodeList = outputRoot.getElementsByTagNameNS(mNamespace, OUT_ELEMENT);

                for (int j = 0; j < nodeList.getLength(); j++) {
                    Node node = nodeList.item(j);
                    Element outElement = (Element) node;
                    Element element = doc.createElement(outputName);
                    outElement.appendChild(element);
                    element.appendChild(doc.createTextNode(outputValue));
                }
            }
        }

    }

    /** Creates a new instance of DTELEngine */
    public DTELEngine() {
        mRoot = new LinkedList();
    }
    
    private void populateFieldNames(Node node) {
        Element element = (Element) node;
        NodeList nodeList = element.getElementsByTagName("Cell");
        mFieldNames = new String[nodeList.getLength() + 1];
        mFieldTypes = new String[nodeList.getLength() + 1];
        mParent = new Field[nodeList.getLength() + 1];
        
        int position = 0;
        for (int i = 0; i < nodeList.getLength(); i++, position++) {
            Element e = (Element) nodeList.item(i);
            NodeList nl = e.getElementsByTagName("Data");
            String index = e.getAttribute("ss:Index");
            if (index != null && index.length() > 0) {
                position = Integer.parseInt(index) - 1;
            }
            if (nl.getLength() == 1) {
                Element dataElement = (Element) nl.item(0);
                String value = dataElement.getTextContent();
                mFieldNames[position] = value;
            }
        }
    }
    
    private void processRow(Node node) {
        Field leafField = null;
        Element element = (Element) node;
        NodeList nodeList = element.getElementsByTagName("Cell");
        
        int position = 0;
        int i;
        for (i = 0; i < nodeList.getLength(); i++, position++) {
            Element e = (Element) nodeList.item(i);
            NodeList nl = e.getElementsByTagName("Data");
            String index = e.getAttribute("ss:Index");
            if (index != null && index.length() > 0) {
                if (position != 0) {
                    position = Integer.parseInt(index) - 1;
                    break;
                }
                position = Integer.parseInt(index) - 1;
            }
            if (nl.getLength() == 1) {
                Field f = new Field();
                leafField = f;
                mParent[position] = f;
                f.setName(mFieldNames[position]);
                Element dataElement = (Element) nl.item(0);
                String value = dataElement.getTextContent();
                f.parseData(value);
                mFieldTypes[position] = f.getType();
                if (position > 0) {
                    mParent[position - 1].addSubField(f);
                } else {
                    mRoot.add(f);
                }
            } else {
                break;
            }
        }
        for (; i < nodeList.getLength(); i++, position++) {
            Element e = (Element) nodeList.item(i);
            NodeList nl = e.getElementsByTagName("Data");
            if (nl.getLength() == 1) {
                Element dataElement = (Element) nl.item(0);
                String value = dataElement.getTextContent();
                if (leafField != null) {
                    String type = leafField.addAssignmentRule(mFieldNames[position], value);
                    mFieldTypes[position] = type;
                }
            }
        }
    }
    
    private void print(String prefix, Field field) {
        System.out.print(prefix + field.getName());
        if (field.getExpressionType().equals("ExactNumber")) {
            System.out.print(": " + field.getExpression());
        }
        if (field.getExpressionType().equals("LessThanNumber")) {
            System.out.print(": less than " + field.getExpression());
        }
        if (field.getExpressionType().equals("GreaterThanNumber")) {
            System.out.print(": greater than " + field.getExpression());
        }
        if (field.getExpressionType().equals("NumberRange")) {
            System.out.print(": " + field.getExpressionOne() + ".." + field.getExpressionTwo());;
        }
        if (field.getExpressionType().equals("ExactString")) {
            System.out.print(": " + field.getExpression());
        }
        System.out.print(" -->");
        Set set = field.getAssignmentRules().entrySet();
        
        for (Iterator i = set.iterator(); i.hasNext();) {
            Map.Entry me = (Map.Entry) i.next();
            System.out.print(" " + me.getKey() + "=" + me.getValue());
        }
        System.out.println("");
        
        for (int i = 0; i < field.getSubFields().size(); i++) {
            print(prefix + " ", (Field) field.getSubFields().get(i));
        }
    }

    public void loadDecisionTable(String file) {
        try {
            File f = new File(file);
            mNamespace = f.getName().replace('.', '_');
            mDecisionTableDocument = DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(f);
            mDecisionTableElement = mDecisionTableDocument.getDocumentElement();
            
            NodeList nodeList = mDecisionTableElement.getElementsByTagName("Row");
            populateFieldNames(nodeList.item(0));
            for (int i = 1; i < nodeList.getLength(); i++) {
                processRow(nodeList.item(i));
            }
            
            // Debug
            //for (int i = 0; i < mRoot.size(); i++) {
            //    print("", (Field) mRoot.get(i));
            //}
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    
    private void assign(Element ruleElement, Element output) {
        String outputName = ruleElement.getAttribute("node");
        String outputValue = ruleElement.getAttribute("value");
        
        NodeList nodeList = output.getElementsByTagName(outputName);
        
        for (int i = 0; i < nodeList.getLength(); i++) {
            Node node = nodeList.item(i);
            Element element = (Element) node;
            element.setTextContent(outputValue);
        }
    }
    
    private void recur(Field field, Element input, Document doc) {
        for (int i = 0; i < field.getSubFields().size(); i++) {
            Field f = (Field) field.getSubFields().get(i);
            check(f, input, doc);
        }
        if (field.getSubFields().size() == 0) {
            field.assign(doc);
        }
    }
    
    private boolean eval(String expression, String type, String value) {
        if (type.compareToIgnoreCase("int") == 0) {
            // Check for static check
            Pattern p = Pattern.compile("[0123456789]+");
            Matcher m = p.matcher(expression);
            if (m.matches()) {
                return expression.compareToIgnoreCase(value) == 0;
            }
            
            p = Pattern.compile("<[ ]+[0123456789]+");
            m = p.matcher(expression);
            if (m.matches()) {
                try {
                    String s[] = expression.split("[ ]+");
                    Integer iExp = Integer.decode(s[1]);
                    Integer iVal = Integer.decode(value);
                    return iVal.compareTo(iExp) < 0;
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
            
            p = Pattern.compile(">[ ]+[0123456789]+");
            m = p.matcher(expression);
            if (m.matches()) {
                try {
                    String s[] = expression.split("[ ]+");
                    Integer iExp = Integer.decode(s[1]);
                    Integer iVal = Integer.decode(value);
                    return iVal.compareTo(iExp) > 0;
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
            
            p = Pattern.compile("BETWEEN[ ]+[0123456789]+[ ]+[0123456789]+[ ]*");
            m = p.matcher(expression);
            if (m.matches()) {
                try {
                    String s[] = expression.split("[ ]+");
                    Integer iLowExp = Integer.decode(s[1]);
                    Integer iHighExp = Integer.decode(s[2]);
                    Integer iVal = Integer.decode(value);
                    return iVal.compareTo(iLowExp) > 0 && iVal.compareTo(iHighExp) < 0;
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        } else if (type.compareToIgnoreCase("string") == 0) {
            
        }
        return false;
    }
    
    private void check(Field field, Element input, Document doc) {
        // Get the name of the input field to test.
        String inputTestName = field.getName();
        
        NodeList nodeList = input.getElementsByTagName(inputTestName);
        for (int i = 0; i < nodeList.getLength(); i++) {
            Node node = nodeList.item(i);
            Element element = (Element) node;
            String inputNodeValue = element.getTextContent();
            if (field.eval(inputNodeValue)) {
                recur(field, input, doc);
            }
        }
    }

    public void execute(DOMSource input, DOMSource output) {
        Document doc = (Document) output.getNode();
        Element root = doc.createElement(OUT_ROOT);
        Element message = doc.createElement(OUT_MESSAGE);
        Element element = doc.createElementNS(mNamespace, OUT_ELEMENT);
        element.setPrefix("ns0");
        doc.appendChild(root);
        root.appendChild(message);
        message.appendChild(element);

        Node rn = input.getNode();
        Element inputRoot = null;
        if (rn instanceof Document) {
            inputRoot = ((Document) rn).getDocumentElement();
        } else if (rn instanceof Element) {
            inputRoot = (Element) rn;
        } else {
            return;
        }

        //Element inputRoot = ((Document)input.getNode()).getDocumentElement();
        //Element outputRoot = ((Document)output.getNode()).getDocumentElement();
        for (int i = 0; i < mRoot.size(); i++) {
            check((Field) mRoot.get(i), inputRoot, doc);
        }
    }


    public static void main(String[] args) {
        String mFile=System.getProperty("ALASKA_ROOT") + "/jbi/dtelse/jbiadapter/test/data/riskscore.dtel";
        //String text="<?xml version=\"1.0\" encoding=\"UTF-8\"?><jbi:message xmlns:msgns=\"riskscore_dtel\" type=\"msgns:InputMessage\" version=\"1.0\" xmlns:jbi=\"http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper\"><jbi:part><inElement xmlns=\"riskscore_dtel\"><Health>good</Health><Age>66.0</Age><Sex>Male</Sex></inElement></jbi:part></jbi:message>";
        String text="<?xml version=\"1.0\" encoding=\"UTF-8\"?><jbi:message xmlns:msgns=\"riskscore_dtel\" type=\"msgns:InputMessage\" version=\"1.0\" xmlns:jbi=\"http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper\"><jbi:part><ns0:inElement xmlns:ns0=\"riskscore_dtel\"><Health>good</Health><Age>66.0</Age><Sex>Male</Sex></ns0:inElement></jbi:part></jbi:message>";
        //String mFile = System.getProperty("ALASKA_ROOT") + "/jbi\\test\\dtel\\src\\untitled.dtel";
        //String text = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><dtelOperation><input><ns0:inElement xmlns:ns0=\"untitled_dtel\"><CreditScore>601.0</CreditScore><Sex>male</Sex><LoanAmount>601.0</LoanAmount><Income>601.0</Income></ns0:inElement></input></dtelOperation>";

        try {
            DTELEngine mDTELEngine = new DTELEngine();
            mDTELEngine.loadDecisionTable(mFile);
            System.out.println("---> Loaded: "+mDTELEngine);

            DOMSource input = new DOMSource(XmlUtil.createDocumentFromXML(true, text));
            DOMSource output =new DOMSource(XmlUtil.createDocument(true));

            Element rootNode = ((Document)input.getNode()).getDocumentElement();
            System.out.println("InputXML: " + XmlUtil.toXml(rootNode, "UTF-8", false));

            mDTELEngine.execute(input, output);

            System.out.println("---> Processed: ");
            Document xdoc2 = (Document)output.getNode();
            Element rootNode2 = xdoc2.getDocumentElement();
            System.out.println("OutputXML: " + XmlUtil.toXml(rootNode2, "UTF-8", false));

        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
