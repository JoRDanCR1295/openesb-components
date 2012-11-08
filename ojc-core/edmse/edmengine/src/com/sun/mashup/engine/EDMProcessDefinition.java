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
package com.sun.mashup.engine;

import com.sun.sql.framework.exception.BaseException;
import com.sun.jbi.internationalization.Messages;
import java.util.logging.Level;
import java.io.File;
import java.util.List;
import java.util.LinkedList;

import com.sun.mashup.engine.utils.XMLFile;
import com.sun.sql.framework.jdbc.DBConnectionParameters;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;
import com.sun.sql.framework.jdbc.SQLPart;
import com.sun.sql.framework.utils.RuntimeAttribute;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * 
 * This class is responsible for representing the mashup queries, 
 * basically picks up the mashup engine file and sucks in the sqls in the 
 * order. Basically, creates the db links, creates the external tables
 * and the view query sqls. 
 *
 *  <MashupEngine>
<connectiondef name="IDB_CONN_DEF" driverName="org.axiondb.jdbc.AxionDriver" dbName="Internal" dbUrl="jdbc:axiondb:testdb:C:/test/" userName="sa" password="02C820">
 *  </connectiondef>
 *   <init>
 *        <sqplPart>
 *             <sql></sql>
 *       </sqlPart>
 *       ...
 *       <sqplPart>
 *             <sql></sql>
 *       </sqlPart>
 *   </init>
 *   <process>
 *      <DataMashup resptype="WEBROWSET">
 *          <sqlPart/>
 *       </DataMashup>
 *   </process>
 *  </MashupEngine>
 *    
 * @author Srinivasan Rengarajan
 * 
 */
public class EDMProcessDefinition {

    private static Logger logger = Logger.getLogger(EDMProcessDefinition.class.getName());
    private static final Messages mMessages = Messages.getMessages(EDMProcessDefinition.class);
    protected DBConnectionParameters connectionDef = null;
    private List<SQLPart> initSQLs = new LinkedList<SQLPart>();
    private Map<String, RuntimeAttribute> attributeMap = new HashMap<String, RuntimeAttribute>();
    private SQLPart mashupQuery = null;
    private XMLFile xmlFile = null;
    private String mashupResponse = "";
    private static final String SQL_PART = "sqlPart";
    private static final String MASHUP_SQL = "mashupsql";
    private static final String MASHUP = "DataMashup";
    private static final String INIT = "init";
    private static final String RTINPUT = "runtimeInputs";
    private static final String RTATTR = "RuntimeAttr";
    public static final String RESPONSETYPE = "resptype";
    public String respValue = null;

    public EDMProcessDefinition(File file) {
        if (file == null) {
            logger.info(mMessages.getString("EDMSE-I0439.Invalid_File"));
        }
        logger.fine(mMessages.getString("EDMSE-F0218.engineFile_Path") + file.getAbsolutePath());
        xmlFile = new XMLFile(file);
        suckInQueries();
    }

    public EDMProcessDefinition() {
    }

    private void suckInQueries() {

        NodeList initNode = xmlFile.getElementByTagName(INIT);
        NodeList sqlPartNodes = ((Element) initNode.item(0)).getElementsByTagName(SQL_PART);
        logger.fine(mMessages.getString("EDMSE-F0219.initStatements") + sqlPartNodes.getLength());
        for (int i = 0; i < sqlPartNodes.getLength(); i++) {
            Node query = (Node) sqlPartNodes.item(i);
            SQLPart sqlp = null;
            try {
                sqlp = new SQLPart((Element) query);
                Logger.getLogger(EDMProcessDefinition.class.getName()).finest("Init Statement ====> " + sqlp.getSQL());
            } catch (BaseException ex) {
                Logger.getLogger(EDMProcessDefinition.class.getName()).log(Level.SEVERE, null, ex);
            }
            if (sqlp != null) {
                this.initSQLs.add(sqlp);
            }
        }

        NodeList mashupNode = xmlFile.getElementByTagName(MASHUP_SQL);
        NodeList mPartNode = ((Element) mashupNode.item(0)).getElementsByTagName(SQL_PART);
        try {
            mashupQuery = new SQLPart((Element) mPartNode.item(0));
        } catch (BaseException ex) {
            Logger.getLogger(EDMProcessDefinition.class.getName()).log(Level.SEVERE, null, ex);
        }
        NodeList resp_type = xmlFile.getElementByTagName(MASHUP);
        Node resp_type_node = (Node) resp_type.item(0);
        mashupResponse = xmlFile.getAttributeFrom((Element) resp_type_node, RESPONSETYPE, true);

        NodeList rtInput = xmlFile.getElementByTagName(RTINPUT);
        if (rtInput != null) {
            Node inputNode = (Node) rtInput.item(0);
            if (inputNode != null) {
                NodeList rtAttrs = xmlFile.getElementByTagName(RTATTR);
                if (rtAttrs != null) {
                    for (int i = 0; i < rtAttrs.getLength(); i++) {
                        Element rtAttribute = (Element) rtAttrs.item(i);
                        RuntimeAttribute ra = new RuntimeAttribute();
                        try {
                            ra.parseXMLString(rtAttribute);
                            this.attributeMap.put(ra.getAttributeName(), ra);
                        } catch (BaseException ex) {
                            Logger.getLogger(EDMProcessDefinition.class.getName()).log(Level.SEVERE, null, ex);
                        }
                    }
                }
            }
        }
    }

    public void setAttributeMap(Map<String, RuntimeAttribute> attributeMap) {
        this.attributeMap = attributeMap;
    }

    public List<SQLPart> getInitSQLParts() {
        return this.initSQLs;
    }

    public void setInitSQLParts(List<SQLPart> newInitSQLs) {
        this.initSQLs = newInitSQLs;
    }

    public Map<String, RuntimeAttribute> getAttributeMap() {
        return attributeMap;
    }

    public void syncQueries() {
    }

    public SQLPart getDataMashupQuery() {
        return this.mashupQuery;
    }

    public void setDataMashupQuery(SQLPart mashupQuery) {
        this.mashupQuery = mashupQuery;
    }

    public String getMashupResponse() {
        return this.mashupResponse;
    }

    public void setMashupResponse(String mashupResponse) {
        this.mashupResponse = mashupResponse;
    }

    public DBConnectionParameters getDBConnectionParams() {
        return this.connectionDef;
    }

    public String dumpQueries() {
        StringBuffer output = new StringBuffer(1000);
        output.append(connectionDef.toString());
        for (int i = 0; i < initSQLs.size(); i++) {
            output.append(initSQLs.get(i));
        }
        output.append(mashupQuery);
        return output.toString();
    }

    public static void main(String[] args) {
        File f = new File(args[0]);
        EDMProcessDefinition processDef = new EDMProcessDefinition(f);
        logger.fine(processDef.dumpQueries());
    }
}
