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

package com.sun.jbi.mm;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.xpath.XPathExpression;


/**
 * Custom configuration properties file
 *   - contains properties for NDC Logging, Message Tracking (checkpoint)
 *   - currently only service endpoint level is supported
 *
 * @author ylee
 *      - added NDC API support
 *      - added additonal ctor that takes a config-file-location
 *      - added getSolutionGroup() method to MonitorConfig
 * @author sunsoabi_edwong
 */
public class CustomConfiguration {

    private Properties props;
    private MonitorConfig monitorConfig;
    private NdcConfig ndcConfig;
    private String propsLocation;

    public static final String MONITOR = "monitor";                     //NOI18N
    public static final String NDC = "ndc";                             //NOI18N
    public static final String GFESBCUSTOM_PROPS_FILE =
            "gfesbcustom.properties";                                   //NOI18N

    public enum PayloadEnum {
        YES("yes"),                                                     //NOI18N
        NO("no"),                                                       //NOI18N
        DIFF("diff"),                                                   //NOI18N
        SELF("self");                                                   //NOI18N
        
        private static final long serialVersionUID = 5776829977024969836L;
        private String enumVal;
        private PayloadEnum(String enumVal) {
            this.enumVal = enumVal;
        }

        @Override
        public String toString() {
            return enumVal;
        }

        public static PayloadEnum toEnum(String enumVal) {
            for (PayloadEnum pe : values()) {
                if (pe.toString().equalsIgnoreCase(enumVal)) {
                    return pe;
                }
            }
            return SELF;
        }
    }

    private static Logger logger =
            Logger.getLogger(CustomConfiguration.class.getName());

    public CustomConfiguration(boolean isLocal) {
        super();
        props = new Properties();
        if (!isLocal) {
            propsLocation = System.getProperty(MMUtil.GFESBCUSTOM_PROPS_KEY);
            init();
        } else {
            logger.fine(
                    "Reading CustomConfiguration from resource stream");//NOI18N
            InputStream is = CustomConfiguration.class.getResourceAsStream(
                    GFESBCUSTOM_PROPS_FILE);
            try {
                props.load(is);
                logger.fine("CustomConfiguration Properties size: "     //NOI18N
                        + props.toString());
            } catch (IOException ex) {
                Logger.getLogger(CustomConfiguration.class.getName())
                        .log(Level.SEVERE, null, ex);
            }
        }

        monitorConfig = new MonitorConfig();
        ndcConfig = new NdcConfig();

    }

    public CustomConfiguration(String configFileLocation) {
        propsLocation = configFileLocation;
        props = new Properties();
        init();
        monitorConfig = new MonitorConfig();
        ndcConfig = new NdcConfig();
    }


    private void init() {
        InputStream is = null;
        try {
            if (null == propsLocation) {
                return;
            }
            File propsFile = new File(propsLocation);
            if (!propsFile.exists()) {
                return;
            }
            is = new FileInputStream(propsFile);
            props.load(is);

        } catch (FileNotFoundException fnfe) {
            // ignore
        } catch (IOException ioe) {
            // ignore
        } finally {
            if (is != null) {
                try {
                    is.close();
                } catch (IOException ioe) {
                    // ignore
                }
            }
        }
    }

    /**
     * refresh config properties from mbean
     * @param configProps
     */
    public void refreshConfig(Properties configProps) {
        if ( configProps!=null ) {
            props.clear();
            props.putAll(configProps);
            monitorConfig = new MonitorConfig();
            ndcConfig = new NdcConfig();
        }
    }

    /**
     * return the configuraiton properties
     * @return
     */
    public Properties getConfigProperties() {
        return props;
    }

    public MonitorConfig getMonitorConfig() {
        return monitorConfig;
    }

    public NdcConfig getNdcConfig() {
        return ndcConfig;
    }

    public class NdcConfig {

        /**
         * check if NDC is enabled for component'serviceInstance (endpoint)
         * @param componentName Component name.
         * @param serviceInstanceID Service instance ID.
         * @return
         */
        public boolean isNdcEnabled(String componentName,
                String serviceInstanceID) {
            boolean ndcEnabled = false;
            if ((componentName != null) && (serviceInstanceID != null)) {
                String propValue = props.getProperty(componentName + "."//NOI18N
                        + serviceInstanceID + "." + NDC + ".enabled");  //NOI18N
                if (propValue != null) {
                    ndcEnabled = Boolean.parseBoolean(propValue);
                }
            }

            return ndcEnabled;
        }

        /**
         * Gets the solution group for NDC gathering.
         * @param componentName Component name.
         * @param serviceInstanceID Service instance ID.
         * @return
         */
        public String getSolutionGroup(String componentName,
                String serviceInstanceID) {
            return (String) props.getProperty(
                    componentName + "." + serviceInstanceID + "." + NDC //NOI18N
                    + ".solutiongroup");                                //NOI18N
        }
    }

    public class MonitorConfig {

        private HashMap <String,HashMap<String, Component>> groupComponents =
                new HashMap();
        private HashMap <String,HashMap<String, Flow>> groupFlows =
                new HashMap();
        private HashMap <String, MessageColumn> messageColumns =
                new HashMap<String, MessageColumn>();
        private String[] solutionGroups;
        private String currentCheckPointDatasource;
        private String currentAtnaDatasource;

        private String archiveCheckPointDatasource;
        private String logpath;

        public HashMap<String, MessageColumn> getMessageColumns() {
            return messageColumns;
        }

        public void setMessageColumns(
                HashMap<String, MessageColumn> messageColumns) {
            this.messageColumns = messageColumns;
        }

        public String getLogpath() {
            return logpath;
        }

        public void setLogpath(String logpath) {
            this.logpath = logpath;
        }


        public String getArchiveCheckPointDatasource() {
            return archiveCheckPointDatasource;
        }

        public void setArchiveCheckPointDatasource(
                String archiveCheckPointDatasource) {
            this.archiveCheckPointDatasource = archiveCheckPointDatasource;
        }

        public String getCurrentCheckPointDatasource() {
            return currentCheckPointDatasource;
        }

        public void setCurrentCheckPointDatasource(
                String currentCheckPointDatasource) {
            this.currentCheckPointDatasource = currentCheckPointDatasource;
        }

        public String getCurrentAtnaDatasource() {
            return currentAtnaDatasource;
        }

        public void setCurrentAtnaDatasource(String currentAtnaDatasource) {
            this.currentAtnaDatasource = currentAtnaDatasource;
        }


        public  MonitorConfig(){
            try {
                if ( !props.isEmpty() ) {
                    initialize();
                }
            } catch(Exception e) {
                e.printStackTrace();
            }
        }

        private void initialize(){
            String key = MONITOR + ".solutiongroups.size";              //NOI18N
            logger.fine("key: " + key + " = " + props.getProperty(key));//NOI18N
            try {
                int size = Integer.parseInt(props.getProperty(key));
                solutionGroups = new String[size];
                for (int i = 0; i < size; i++) {
                    String group = props.getProperty(MONITOR
                            + ".solutiongroup." + (i + 1));             //NOI18N
                    solutionGroups[i] = group;
                    initComponents(group);
                    initFlows(group);
                    initDatasource();
                    initMisc(group);
                    initMessageColumns(group);
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }

        public void initMessageColumns(String solutionGroup){
             String monitorSolutionGroup =
                     MONITOR + "." + solutionGroup + ".";               //NOI18N
            int size = Integer.parseInt(props.getProperty(monitorSolutionGroup
                    + "message.display.columns.count", "0"));           //NOI18N
             for (int i = 1; i <= size; i++) {
                 String columnName=props.getProperty(monitorSolutionGroup
                         + "message.display.column." + i + ".name");    //NOI18N
                 String xpath = props.getProperty(monitorSolutionGroup
                         + "message.display.column." + i + ".xpath");   //NOI18N
                 MessageColumn column =new MessageColumn(columnName,xpath);
                 messageColumns.put("" + i, column);                    //NOI18N
            }
        }

        public void initMisc(String solutionGroup){
            setLogpath(props.getProperty(MONITOR + "." + solutionGroup  //NOI18N
                    + ".log.path"));                                    //NOI18N
        }
        public void initDatasource(){
            setCurrentCheckPointDatasource(props.getProperty(
                    MONITOR + ".dbcp.jndi.current"));                   //NOI18N
            setCurrentAtnaDatasource(props.getProperty(MONITOR
                    + ".dbatna.jndi.current"));                         //NOI18N
            setArchiveCheckPointDatasource(props.getProperty(MONITOR
                    + ".dbcp.jndi.archive"));                           //NOI18N
            setLogpath(props.getProperty(MONITOR
                    + ".dbcp.jndi.archive"));                           //NOI18N
        }
        private void initSolutionGroups(){
             int size = Integer.parseInt(props.getProperty(MONITOR
                     + ".solutiongroups.size"));                        //NOI18N
             solutionGroups = new String[size];
             for(int i = 0; i < size; i++ ){
                 solutionGroups[i] = props.getProperty(MONITOR
                         + ".solutiongroup." + (i + 1));                //NOI18N
             }
        }

        private void initComponents(String solutionGroup) {
            String monitorSolutionGroup =
                    MONITOR + "." + solutionGroup + ".";                //NOI18N
            int size = Integer.parseInt(props.getProperty(
                    monitorSolutionGroup + "components.size", "0"));    //NOI18N
            HashMap <String,Component> components = new HashMap();
            for (int i = 1; i <= size; i++) {

                String key = monitorSolutionGroup + "component." + i;   //NOI18N
                String component = props.getProperty(key);
                String serviceInstanceID = component;
                String display = props.getProperty(key + ".display");   //NOI18N
                String imageURL = props.getProperty(key + ".image.url");//NOI18N
                String payload = props.getProperty(key + ".payload");   //NOI18N
                components.put("" + i, new Component(i,                 //NOI18N
                        serviceInstanceID, display, imageURL, payload));
            }
            groupComponents.put(solutionGroup, components);
        }

        private void initFlows(String solutionGroup) {
            String monitorSolutionGroup =
                    MONITOR + "." + solutionGroup + ".";                //NOI18N
            int size = Integer.parseInt(props.getProperty(
                    monitorSolutionGroup + "flows.size", "0"));         //NOI18N
            HashMap <String,Flow> flows=new HashMap<String,Flow>();
            for (int i = 1; i <= size; i++) {
                String key = monitorSolutionGroup + "flow." + i;        //NOI18N
                String displayStr = props.getProperty(key + ".display");//NOI18N
                Flow flow = new Flow(displayStr);
                int linkSize = Integer.parseInt(props.getProperty(key
                        + ".links.size"));                              //NOI18N
                for (int j = 1; j <= linkSize; j++){
                    String lstr=props.getProperty(key + ".link." + j);  //NOI18N
                    Link link=new Link(lstr);
                    flow.addLink(j, link);
                }

                flows.put(""+i,flow);
            }
            groupFlows.put(solutionGroup,flows);
        }

        public HashMap<String, HashMap<String, Component>> getGroupComponents() {
            return groupComponents;
        }

        public void setGroupComponents(
                HashMap<String, HashMap<String, Component>> groupComponents) {
            this.groupComponents = groupComponents;
        }

        public HashMap<String, HashMap<String, Flow>> getGroupFlows() {
            return groupFlows;
        }

        public void setGroupFlows(
                HashMap<String, HashMap<String, Flow>> groupFlows) {
            this.groupFlows = groupFlows;
        }

        public String[] getSolutionGroups() {
            return solutionGroups;
        }

        public void setSolutionGroups(String[] solutionGroups) {
            this.solutionGroups = solutionGroups;
        }

        public boolean isMonitorEnabled(String componentName,
                String serviceInstanceID) {
            boolean monitorEnabled = false;
            if ((componentName != null) && (serviceInstanceID != null)) {
                String propValue = props.getProperty(componentName + "."//NOI18N
                    + serviceInstanceID + "." + MONITOR + ".enabled");  //NOI18N
                if (propValue != null) {
                    monitorEnabled = Boolean.parseBoolean(propValue);
                }
            }
            return monitorEnabled;
        }

        public String getSolutionGroup(String componentName,
                String serviceInstanceID) {
            return (String)props.getProperty(
                    componentName + "." + serviceInstanceID + "."       //NOI18N
                    + MONITOR + ".solutiongroup");                      //NOI18N
        }

        public String getMessageTrackingIDModeInbound() {
            return (String)props.getProperty("monitor.trackid.inbound");//NOI18N
        }

        public XPathExpression getMessageTrackingXPathExpression(
                String componentName, String serviceInstanceId,
                String operation) {
            String xpathStr = (String)props.getProperty(componentName
                    + "." + serviceInstanceId + "." + operation         //NOI18N
                    + "." + "monitor.trackid.inbound");                 //NOI18N
            return XPathExpressionSupport.computeXPathExpression(xpathStr);
        }

        public class MessageColumn{
            private String columnName;
            private String xpath;

       


            public MessageColumn(String columnName, String xpath) {
                this.columnName = columnName;
                this.xpath = xpath;
            }

            public String getColumnName() {
                return columnName;
            }

            public void setColumnName(String columnName) {
                this.columnName = columnName;
            }

            public String getXpath() {
                return xpath;
            }

            public void setXpath(String xpath) {
                this.xpath = xpath;
            }

        }


        public List<String> getOperationNames(String componentName,
                String serviceInstanceId) {
            List<String> opNames = new ArrayList<String>();
            String prefix = componentName + "." + serviceInstanceId;    //NOI18N
            String suffix = ".monitor.trackid.inbound";                 //NOI18N
            for ( Iterator iter = props.keySet().iterator(); iter.hasNext(); ) {
                String key = (String)iter.next();
                if ( key.startsWith(prefix) && key.endsWith(suffix)) {
                     String opName = key.substring(prefix.length() + 1,
                             key.indexOf(suffix));
                     opNames.add(opName);
                }
            }
            return opNames;
        }


        public class Component {
            private int index;
            private String componentid;
            private String serviceInstanceID;
            private String display;
            private String imageURL;
            private PayloadEnum payload;

            public String getComponentid() {
                return componentid;
            }

            public void setComponentid(String componentid) {
                this.componentid = componentid;
            }

            public int getIndex() {
                return index;
            }

            public void setIndex(int index) {
                this.index = index;
            }

            public Component(int index, String serviceInstanceID,
                    String display, String imageURL) {
                this(index, serviceInstanceID, display, imageURL, null);
            }

            public Component(int index, String serviceInstanceID,
                    String display, String imageURL, String payload) {
                super();
                if (serviceInstanceID.indexOf(".") > 0 ){               //NOI18N
                    componentid = serviceInstanceID.substring(
                            0, serviceInstanceID.indexOf("."));         //NOI18N
                    this.serviceInstanceID = serviceInstanceID.substring(
                            serviceInstanceID.indexOf(".") + 1);        //NOI18N
                } else {
                     componentid = serviceInstanceID;
                     this.serviceInstanceID = serviceInstanceID;
                }
                this.display = display;
                this.imageURL=imageURL;
                this.index = index;
                this.payload = PayloadEnum.toEnum(payload);
            }

            public String getServiceInstanceID() {
                return serviceInstanceID;
            }

            public String getDisplay() {
                return display;
            }

            public String getImageURL() {
                return imageURL;
            }

            public PayloadEnum getPayload() {
                return payload;
            }
        }

        public class Flow {
            private String display;
            private HashMap<String, Link> links=new HashMap<String, Link>();

            public Flow(String display) {
                super();
                this.display = display;
            }
            public void addLink(int index, Link link){
                links.put("" + index, link);                            //NOI18N
            }

            public String getDisplay() {
                return display;
            }

            public void setDisplay(String display) {
                this.display = display;
            }

            public HashMap<String, Link> getLinks() {
                return links;
            }

            public void setLinks(HashMap<String, Link> links) {
                this.links = links;
            }

        }


        public class Link {
                private int from;
                private int to;

                public Link(String str) {
                    super();
                    valueOf(str);

                }

                public void valueOf(String str) {
                    int sep = str.indexOf('>');                         //NOI18N
                    from = Integer.parseInt(str.substring(0, sep).trim());
                    to = Integer.parseInt(str.substring(sep + 1).trim());
                }

                public int getFrom() {
                    return from;
                }

                public int getTo() {
                    return to;
                }

            @Override
            public String toString() {
                return from + ">" + to;                                 //NOI18N
            }

            }

    }

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) throws Throwable {
//        CustomConfiguration cc = new CustomConfiguration(false);
//        String componentName = args[0];
//        String serviceInstanceID = args[1];
//        System.out.println("monitorEnabled="
//                + cc.getMonitorConfig().isMonitorEnabled(
//                        componentName, serviceInstanceID));

        CustomConfiguration config = new CustomConfiguration(
                System.getProperty("user.dir")                          //NOI18N
                + "/src/org/glassfish/openesb/customconfig/gfesbcustom.properties");    //NOI18N
        XPathExpression xpathInfo = config.getMonitorConfig()
                .getMessageTrackingXPathExpression("sun-http-binding",  //NOI18N
                "{urn:ihe:iti:pixv3:2007}PIXConsumer_Service,PIXConsumer_Port_Soap12",  //NOI18N
                "PIXManager_PRPA_IN201301UV02");                        //NOI18N
        System.out.println("xpathInfo: "+xpathInfo);                    //NOI18N

//        List<String> opNames = config.getMonitorConfig().getOperationNames(
//              "sun-http-binding",                                     //NOI18N
//              "{urn:ihe:iti:pixv3:2007}PIXConsumer_Service,PIXConsumer_Port_Soap12"); //NOI18N
//        System.out.println(opNames);

  }
}
