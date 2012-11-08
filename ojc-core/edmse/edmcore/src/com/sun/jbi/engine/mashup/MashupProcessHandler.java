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
package com.sun.jbi.engine.mashup;

import com.sun.jbi.engine.mashup.exception.EDMApplicationException;
import com.sun.jbi.engine.mashup.mbean.MashupSERuntimeConfigurationMBean;
import com.sun.mashup.engine.EDMProcessDefinition;
import com.sun.mashup.engine.MashupEngine;
import com.sun.mashup.engine.impl.MashupEngineImpl;
import com.sun.mashup.engine.response.ResponseTypes;
import com.sun.mashup.engine.response.Serializer;
import com.sun.mashup.engine.response.SerializerFactory;
import com.sun.jbi.management.descriptor.SUDescriptorSupport;
import com.sun.jbi.management.descriptor.Provides;
import com.sun.jbi.management.descriptor.ConfigurationException;
import com.sun.jbi.internationalization.Messages;
import com.sun.mashup.engine.QueryContext;
import com.sun.rowset.WebRowSetImpl;
import com.sun.sql.framework.jdbc.SQLPart;
import com.sun.sql.framework.jdbc.SQLUtils;
import com.sun.sql.framework.utils.RuntimeAttribute;
import com.sun.sql.framework.utils.StringUtil;
import java.io.File;
import java.io.StringWriter;
import java.sql.ResultSet;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.TimeZone;
import java.util.logging.Logger;
import javax.wsdl.Definition;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import javax.management.openmbean.CompositeData;
import org.axiondb.ExternalConnectionProvider;

/**
 * This class is responsible for starting the Mashup engine process, and retrieving
 * the results once the engine completes processing or get an exception
 *
 * @author Sun Microsystems
 *
 */
public class MashupProcessHandler {

    private static Logger logger = Logger.getLogger(MashupProcessHandler.class.getName());
    private static final Messages mMessages = Messages.getMessages(MashupProcessHandler.class);
    private List inputArgs;
    private MashupMapEntry processContext = null;
    private MashupSERuntimeConfigurationMBean runtimeConfig = null;
    private QueryContext queryContext = null;
    String mashupResponse = null;
    public String responseType = null;

    public MashupProcessHandler(MashupMapEntry entry, List inputArgs,
            MashupSERuntimeConfigurationMBean runtimeConfig, QueryContext queryContext) {

        // added QueryContext to constructor
        this.inputArgs = inputArgs;
        this.processContext = entry;
        this.runtimeConfig = runtimeConfig;
        this.queryContext = queryContext;
        try {
            System.setProperty(ExternalConnectionProvider.EXTERNAL_CONNECTION_PROVIDER_PROPERTY_NAME, runtimeConfig.getExternalConnectionProvider());
        } catch (Throwable e) {
            System.setProperty(ExternalConnectionProvider.EXTERNAL_CONNECTION_PROVIDER_PROPERTY_NAME, "com.sun.mashup.engine.utils.JndiConnectionProvider");
        }

    }

    private void setConnectionProvider() {
    }

    private void overrideProcessDefinition(EDMProcessDefinition edmDef, String engineFilePath) throws Exception {
        String configName = null;
        Map appConfigMap = runtimeConfig.retrieveApplicationConfigurationsMap();
        this.queryContext.setWorkingDirectory(runtimeConfig.getAxiondbWorkingDirectory());
        Map<String, RuntimeAttribute> attributeMap = edmDef.getAttributeMap();
        List names = new ArrayList(), values = new ArrayList();
        for (String name : attributeMap.keySet()) {
            names.add(name);
            RuntimeAttribute ra = attributeMap.get(name);
            values.add(ra.getAttributeValue());
        }

        if (appConfigMap != null && appConfigMap.size() > 0) {
            File engineFile = new File(engineFilePath);
            String pathToJbiXml = engineFile.getParent();
            SUDescriptorSupport suDescriptor = new SUDescriptorSupport(pathToJbiXml);
            Provides[] provides = suDescriptor.getProvides();
            if (provides != null && provides.length > 0) {
                configName = provides[0].getApplicationConfigurationName();
                try {
                    AppConfigValidationUtil.validateApplicationConfigName(runtimeConfig, configName);
                } catch (EDMApplicationException e) {
                    Logger.getLogger(MashupProcessHandler.class.getName()).fine(mMessages.getString("EDMSE-F0204.error_Resolving_AppConfig"));
                }

                CompositeData row = (CompositeData) appConfigMap.get(configName);
                if (row != null) {
                    String axiondbWorkingDir = (String) row.get("WorkingDir");
                    if (axiondbWorkingDir == null || "".equalsIgnoreCase(axiondbWorkingDir) || !(new File(axiondbWorkingDir).exists())) {
                        axiondbWorkingDir = runtimeConfig.getAxiondbWorkingDirectory();
                        this.queryContext.setWorkingDirectory(axiondbWorkingDir);
                    }
                    String axiondbDataDir = (String) row.get("DataDir");
                    if (axiondbDataDir == null || "".equalsIgnoreCase(axiondbDataDir) || !(new File(axiondbDataDir).exists())) {
                        axiondbDataDir = runtimeConfig.getAxiondbDataDirectory();
                    }

                    String dynamicFileFlag = (String) row.get("DynamicFlatFile");
                    boolean isDynamicFlatFile = false;
                    if (dynamicFileFlag != null) {
                        isDynamicFlatFile = Boolean.parseBoolean(dynamicFileFlag);
                    }

                    if (isDynamicFlatFile) {
                        this.overrideCreateSqlTokens(edmDef, names, queryContext.getDynamicValues());
                    } else {
                        List dataFilePaths = new ArrayList();
                        for (Object fileName : queryContext.getDynamicValues()) {
                            dataFilePaths.add(axiondbDataDir + File.separator + (String) fileName);
                        }
                        this.overrideCreateSqlTokens(edmDef, names, dataFilePaths);
                    }
                    runtimeOverrideDBLink(row, edmDef);
                } else {
                    this.overrideCreateSqlTokens(edmDef, names, values);
                }
            } else {
                this.overrideCreateSqlTokens(edmDef, names, values);
            }
        } else {
            Logger.getLogger(MashupProcessHandler.class.getName()).info(mMessages.getString("EDMSE-F0204.error_Resolving_AppConfig"));
            this.overrideCreateSqlTokens(edmDef, names, values);
        }
    }

    private List<String> overrideCreateSqlTokens(EDMProcessDefinition edmproDef, List<String> tokenNames, List<String> tokenValues) {
        int noOfKeys = tokenNames.size();
        int noOfValues = tokenValues.size();

        String[] victims = new String[noOfKeys];
        String[] replacements = new String[noOfValues];
        int i = 0, j = 0;

        for (String victim : tokenNames) {
            victims[i++] = "{" + victim + "}";
        }

        for (String value : tokenValues) {
            replacements[j++] = value;
        }

        List<String> newSqls = new ArrayList<String>();
        List<SQLPart> sqls = edmproDef.getInitSQLParts();
        String newSQL = null;
        int index = 0;
        for (index = 0; index < sqls.size(); index++) {
            SQLPart aPart = sqls.remove(index);
            if (aPart.getType().equalsIgnoreCase("createFlatfileStatement") ||
                    aPart.getType().equalsIgnoreCase("createExternalStatement")) {
                newSQL = StringUtil.replaceInString(aPart.getSQL(), victims, replacements);
                Logger.getLogger(MashupProcessHandler.class.getName()).fine(mMessages.getString("EDMSE-F0205.create_SQL") + newSQL);
                newSqls.add(newSQL);
                aPart.setSQL(newSQL);
            }
            sqls.add(index, aPart);
        }

        edmproDef.setInitSQLParts(sqls);

        return newSqls;
    }

    private String extractDriverClass(String dbLinkString) {
        String[] strs = dbLinkString.split("DRIVER='");
        String[] strs1 = strs[1].split("'");
        String driver = strs1[0].trim();
        return driver;
    }

    private List runtimeOverrideDBLink(CompositeData row, EDMProcessDefinition edmproDef) throws Exception {
        javax.sql.DataSource ds = null;
        String jndiResName = "";
        String linkName = "";
        List destLinks = new ArrayList();
        int index = 0;
        List<SQLPart> sqls = edmproDef.getInitSQLParts();
        for (index = 0; index < sqls.size(); index++) {
            SQLPart aPart = sqls.remove(index);
            if (aPart.getType().equalsIgnoreCase("createDbLinkStatement")) {
                String dbLink = aPart.getSQL();
                String newDbLinkSql = null;
                for (int i = 1; i <= 5; i++) {
                    linkName = "SOURCECONNECTION" + i;

                    if (dbLink.indexOf(linkName) != -1) {
                        jndiResName = (String) row.get("SourceConnection" + i);
                        break;
                    }
                }
                if (jndiResName != null && !"".equalsIgnoreCase(jndiResName)) {
                    try {
                        newDbLinkSql = SQLUtils.createDBLinkSQL(linkName, jndiResName);
                        destLinks.add(newDbLinkSql);
                    } catch (Exception e) {
                        //Logger.printThrowable(Logger.ERROR, LOG_CATEGORY, this, DN + "InitTask:runtimeOverrideDBLink", e);
                    }
                } else {
                    destLinks.add(dbLink);
                }

                Logger.getLogger(MashupProcessHandler.class.getName()).fine(mMessages.getString("EDMSE-F0206.create_DbLinkSQL")+ newDbLinkSql);
                aPart.setSQL(newDbLinkSql);
            }
            sqls.add(index, aPart);
        }

        edmproDef.setInitSQLParts(sqls);
        return destLinks;
    }

    private void setQueryContext(Map<String, RuntimeAttribute> runtimeAttributeMap) {
        int i = 0;
        int pageNumber = 0;
        try {
            pageNumber = Integer.parseInt((String) inputArgs.get(i++));
        } catch (Throwable e) {
            MashupProcessHandler.logger.fine(mMessages.getString("EDMSE-F0207.inValid_PageSize"));
        }
        int row = -1;
        try {
            row = Integer.parseInt((String) inputArgs.get(i++));
        } catch (Throwable e) {
            MashupProcessHandler.logger.fine(mMessages.getString("EDMSE-F0208.inValid_Row"));
        }

        String colId = "";

        try {
            colId = (String) inputArgs.get(i++);
            if(colId.equals("EMPTYNODE"))
                colId = "";
        } catch (Throwable e) {
            MashupProcessHandler.logger.fine(mMessages.getString("EDMSE-F0209.inValid_Columnid"));
        }

        // this.queryContext.setPageSize(pageSize);
        this.queryContext.setPageNumber(pageNumber);
        this.queryContext.setRow(row);
        this.queryContext.setColumn(colId);
        HashMap<String, String> dynamicMap = new HashMap<String, String>();
        List dynamicParams = new ArrayList(), dynamicValues = new ArrayList();
        for (String key : runtimeAttributeMap.keySet()) {
            dynamicParams.add(key);
            String val = null;
            try {
                 val = (String) inputArgs.get(i++);
            } catch(IndexOutOfBoundsException iex) {
                // If the user does not pass the argument value (empty element)
                // for the runtime argument, we should use default (done in
                // SQLUtils.populatePreparedStatement
            }
            if(val != null) {
                dynamicValues.add(val);
                dynamicMap.put(key, val);
            }
        }
        this.queryContext.setDynamicParams(dynamicParams);
        this.queryContext.setDynamicValues(dynamicValues);
        this.queryContext.setDynamicMap(dynamicMap);
    }

    public String startProcess() throws Exception {
        StringBuffer response = null;
        WebRowSetImpl wrs = null;
        String engineFilePath = processContext.getFile();
        logger.fine(mMessages.getString("EDMSE-F0210.enginefilePath") + engineFilePath);
        if (engineFilePath != null) {
            EDMProcessDefinition edmDef = new EDMProcessDefinition(new File(engineFilePath));
            //edmDef.syncQueries();
            this.setQueryContext(edmDef.getAttributeMap());
            this.overrideProcessDefinition(edmDef, engineFilePath);
            String prefix = extractOutputItemPrefix(engineFilePath);
            responseType = edmDef.getMashupResponse();
            MashupEngine engine = new MashupEngineImpl(edmDef, this.queryContext);

            if (responseType != null) {
                responseType = edmDef.getMashupResponse().trim();
            } else if (responseType == null) {
                responseType = ResponseTypes.JSON;
            }

            try {
                ResultSet rs = engine.exec();
                response = new StringBuffer(1000);
                response.append("<execute>");
                response.append("<");
                response.append(prefix).append("_outputItem").append(" xmlns=\"http://com.sun.jbi/edm/edmengine/\">");

                this.logger.fine(mMessages.getString("EDMSE-F0211.ResponseType")+ responseType);
                if (rs != null) {
                    String payload = generatePayload(responseType, rs, this.queryContext.getColumn(), this.queryContext.getOffset());
                    response.append(payload);
                }
                response.append("</").append(prefix).append("_outputItem").append(">");
                response.append("</execute>");
                if (rs != null) {
                    rs.close();
                }
            } catch (Exception e) {
                this.logger.severe(e.getMessage());
                throw e;
            } finally {
                engine.stopMashupEngine();
            }
        }
        return response.toString();
    }

    private static String extractOutputItemPrefix(String engineFilePath) {
        File f = new File(engineFilePath);
        String fileName = f.getName();
        String prefix = fileName.substring(0, fileName.indexOf("."));
        return prefix;
    }

    private String getTimeStampString(long time) {
        Calendar calendar = new GregorianCalendar(TimeZone.getTimeZone("UTC"));
        calendar.setTimeInMillis(time);
        StringBuffer buffer = new StringBuffer();
        buffer.append(calendar.get(Calendar.YEAR));
        buffer.append("-").append(twoDigit(calendar.get(Calendar.MONTH) + 1));
        buffer.append("-").append(twoDigit(calendar.get(Calendar.DAY_OF_MONTH)));
        buffer.append("T").append(twoDigit(calendar.get(Calendar.HOUR_OF_DAY)));
        buffer.append(":").append(twoDigit(calendar.get(Calendar.MINUTE)));
        buffer.append(":").append(twoDigit(calendar.get(Calendar.SECOND)));
        buffer.append(".").append(twoDigit(calendar.get(Calendar.MILLISECOND) / 10));
        buffer.append("Z");
        return buffer.toString();
    }

    private static String twoDigit(int i) {
        if ((i >= 0) && (i < 10)) {
            return "0" + String.valueOf(i);
        }
        return String.valueOf(i);
    }

    private String generatePayload(String responseType, ResultSet rs, String column, int offset) throws Exception {
        Serializer instance = SerializerFactory.getInstance(responseType);
        return instance.serialize(rs, column, offset);
    }

    /**
     * @param
     * @return
     */
    private StringBuffer createOutputMessageString(WebRowSetImpl wrs) throws Exception {
        StringBuffer sb = new StringBuffer();
        // TODO Relook this piece of code.       
        StringWriter writer = new StringWriter();
        wrs.writeXml(writer);
        Document output = XmlUtil.createDocumentFromXML(true, writer.toString());
        String content = writer.toString();
        XmlUtil.toXml((Node) output.getDocumentElement(), "UTF-8", false);
        content = content.substring(content.indexOf("?>") + 2);
        sb.append(content);
        return sb;
    }
}
