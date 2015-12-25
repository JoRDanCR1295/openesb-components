package net.openesb.jbi.restbc.jbiadapter;

import java.io.File;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.ws.rs.core.MediaType;
import javax.xml.namespace.QName;

import net.openesb.jbi.restbc.jbiadapter.descriptor.EndpointIdentifier;
import net.openesb.jbi.restbc.jbiadapter.descriptor.Provides;
import net.openesb.jbi.restbc.jbiadapter.descriptor.RestSUDescriptor;
import net.openesb.jbi.restbc.jbiadapter.inbound.JaxrsPojoServiceUnit;
import net.openesb.jbi.restbc.jbiadapter.util.PathUtil;
import net.openesb.jbi.restbc.jbiadapter.wsdl.WSDLEndpoint;
import net.openesb.jbi.restbc.jbiadapter.wsdl.WSDLServiceUnitConfig;

import org.apache.xml.resolver.CatalogManager;
import org.apache.xml.resolver.tools.CatalogResolver;
import org.xml.sax.EntityResolver;



/**
 * ServiceUnit.java
 *
 * @author Edward Chou
 */
public class ServiceUnit {
    
    public final static String REST_PROPS_FILE_NAME = "rest.properties";
    public final static String JAXRS_POJO_CONFIG_FILE_NAME = "jaxrs-pojo.properties";
    
    /*
     * 51-60
     */
    private final static Logger logger = Logger.getLogger(ServiceUnit.class.getName());
    
    private RestComponent component;
    private ComponentContext context;
    private String serviceUnitName;
    private String serviceUnitRootPath;
    private RestSUDescriptor suDescriptor;
    private Map<String, File> wsdls = new HashMap<String, File> ();
    private Map<String, File> xsds = new HashMap<String, File> ();
    private EntityResolver resolver = null;
    
    private List<File> jarFiles = new ArrayList<File> ();
    private ClassLoader suClassloader = null;
    
    private List<ServiceUnitConfig> suConfigs = new ArrayList<ServiceUnitConfig> ();
    
    public ServiceUnit(RestComponent component, ComponentContext context, String serviceUnitName, String serviceUnitRootPath) throws Exception {
        this.component = component;
        this.context = context;
        this.serviceUnitName = serviceUnitName;
        this.serviceUnitRootPath = serviceUnitRootPath;
        
        init();
    }
    
    private void init() throws Exception {
        suDescriptor = new RestSUDescriptor(serviceUnitRootPath);
        File rootDir = new File(serviceUnitRootPath);
        
        File catalog = new File(rootDir, "META-INF" + File.separator + "catalog.xml");
        if (catalog.exists()) {
            CatalogManager catalogManager = new CatalogManager();
            catalogManager.setCatalogFiles(catalog.getAbsolutePath());
            catalogManager.setRelativeCatalogs(true);
            catalogManager.setUseStaticCatalog(false);
            resolver = new CatalogResolver(catalogManager);
        }
        
        scanDirectories(rootDir);
        
        initSUClassloader();
    }
    
    private void scanDirectories(File currentDir) {
        File[] filesInCurrentDir = currentDir.listFiles();
        for (File f : filesInCurrentDir) {
            if (f.isFile()) {
                String fileName = f.getName();
                if (fileName.equalsIgnoreCase(REST_PROPS_FILE_NAME)) {
                    try {
                        PropertyServiceUnitConfig suConfig = new PropertyServiceUnitConfig(this, component, context, suDescriptor, f);
                        for (EndpointIdentifier ei : suDescriptor.getServices()) {
                            if (ei.getServiceName().equals(suConfig.getServiceName()) &&
                                    ei.getEndpointName().equals(suConfig.getEndpointName())) {
                                suConfig.setEndpointIdentifier(ei);
                                if (ei instanceof Provides) {
                                    suConfig.parseOutboundConfiguration();
                                } else {
                                    suConfig.parseInboundConfiguration();
                                }
                                break;
                            }
                        }
                        
                        if (suConfig.getEndpointIdentifier() == null) {
                            throw new Exception("unable to resolved endpoint in jbi.xml with property configuration: service-name=" 
                                    + suConfig.getServiceName().toString() + ", endpoint-name=" + suConfig.getEndpointName());
                        }
                        
                        suConfigs.add(suConfig);
                        
                    } catch (Exception e) {
                        String msg = I18n.loc("RESTBC-6051: unable to instantiate PropertyServiceUnitConfig ServiceUnit", e);//NOI18N
                        logger.warning(msg);
                        continue;
                    }
                } else if (fileName.equals(JAXRS_POJO_CONFIG_FILE_NAME)) {
                    try {
                        JaxrsPojoServiceUnit suConfig = new JaxrsPojoServiceUnit(component, context, currentDir.getAbsolutePath(), f);
                        suConfigs.add(suConfig);
                    } catch (Exception e) {
                        String msg = I18n.loc("RESTBC-6052: unable to instantiate JaxrsPojoServiceUnitConfig ServiceUnit", e);//NOI18N
                        logger.warning(msg);
                        continue;
                    }
                } else if (fileName.toLowerCase().endsWith(".wsdl")) {
                    if (wsdls.containsKey(fileName)) {
                        continue;
                    }
                    
                    try {
                        WSDLServiceUnitConfig suConfig = new WSDLServiceUnitConfig(this, component, context, suDescriptor, f, resolver);
                        suConfigs.addAll(suConfig.getWSDLEndpoints());
                        wsdls.put(fileName, f);
                    } catch (Exception e) {
                        String msg = I18n.loc("RESTBC-6053: unable to instantiate WSDLServiceUnitConfig ServiceUnit", e);//NOI18N
                        logger.warning(msg);
                        continue;
                    }
                } else if (fileName.toLowerCase().endsWith(".xsd")) {
                    if (xsds.containsKey(fileName)) {
                        continue;
                    }
                    xsds.put(fileName, f);
                } else if (fileName.toLowerCase().endsWith(".jar")) {
                    jarFiles.add(f);
                }
            } else if (f.isDirectory()) {
                scanDirectories(f);                
            }
        }
    }
    
    private void initSUClassloader() throws Exception {
        List<URL> urls = new ArrayList<URL> ();
        for (File f : jarFiles) {
            urls.add(f.toURL());
        }
        suClassloader = new URLClassLoader(urls.toArray(new URL[]{}), getClass().getClassLoader());
    }
        
    public synchronized void start() throws Exception {
        if (logger.isLoggable(Level.FINEST)) {
            String msg = I18n.lf("RESTBC-1051: ServiceUnit.start() called serviceUnitName={0}", serviceUnitName);//NOI18N
            logger.finest(msg);
        }
        
        for (ServiceUnitConfig suConfig : suConfigs) {
            try {
                suConfig.start();
            } catch (JBIException jbiEx) {
                continue;
            }
        }
    }
    
    public synchronized void stop() throws Exception {
        if (logger.isLoggable(Level.FINEST)) {
            String msg = I18n.lf("RESTBC-1052: ServiceUnit.stop() called serviceUnitName={0}", serviceUnitName);//NOI18N
            logger.finest(msg);
        }
        
        for (ServiceUnitConfig suConfig : suConfigs) {
            try {
                suConfig.stop();
            } catch (JBIException jbiEx) {
                continue;
            }
        }
    }
    
    public synchronized void shutdown() throws Exception {
        if (logger.isLoggable(Level.FINEST)) {
            String msg = I18n.lf("RESTBC-1053: ServiceUnit.shutdown() called serviceUnitName={0}", serviceUnitName);//NOI18N
            logger.finest(msg);
        }
        
        for (ServiceUnitConfig suConfig : suConfigs) {
            try {
                suConfig.shutdown();
            } catch (JBIException jbiEx) {
                continue;
            }
        }
        
        suClassloader = null;
    }
    
    public String getServiceUnitName() {
        return serviceUnitName;
    }
    public String getServiceUnitRootPath() {
        return serviceUnitRootPath;
    }
    
    public synchronized OutboundConfiguration findActivatedEndpointOutbound(QName serviceName, String endpointName, QName operationName) {
        for (ServiceUnitConfig suConfig : suConfigs) {
            if (suConfig instanceof PropertyServiceUnitConfig) {
                PropertyServiceUnitConfig outboundSUConfig = (PropertyServiceUnitConfig) suConfig;
                if (outboundSUConfig.getServiceName().equals(serviceName) &&
                        outboundSUConfig.getEndpointName().equals(endpointName)) {
                    return outboundSUConfig.getOutboundConfiguration();
                }
            } else if (suConfig instanceof WSDLEndpoint) {
                WSDLEndpoint wsdlEndpoint = (WSDLEndpoint) suConfig;
                if (wsdlEndpoint.isOutbound() &&
                        wsdlEndpoint.getServiceName().equals(serviceName) &&
                        wsdlEndpoint.getEndpointName().equals(endpointName)) {
                    OutboundConfiguration outboundConfig = wsdlEndpoint.findOutboundConfig(operationName);
                    if (outboundConfig != null) {
                        return outboundConfig;
                    }
                }
            }
        }
        return null;
    }
    
    /**
     * Give all the inbounds configurations for the serviceUnit
     * 
     * @return serviceUnit inbounds configurations. 
     */
    public synchronized Collection<InboundConfiguration> getInboundsConfigurations(){
    	List<InboundConfiguration> inboundsConfigs = new ArrayList<InboundConfiguration>();
    	
    	   for (ServiceUnitConfig suConfig : suConfigs) {
               if (suConfig instanceof PropertyServiceUnitConfig) {
                   PropertyServiceUnitConfig propertySUConfig = (PropertyServiceUnitConfig) suConfig;
                   if (!propertySUConfig.isOutbound() && propertySUConfig.getInboundConfiguration() != null) {
                	   inboundsConfigs.add(propertySUConfig.getInboundConfiguration());
                   }
                   
               } else if (suConfig instanceof WSDLEndpoint) {
                   WSDLEndpoint wsdlEndpoint = (WSDLEndpoint) suConfig;
                   if (!wsdlEndpoint.isOutbound() &&  wsdlEndpoint.getInboundsConfigs() != null) {
                	   inboundsConfigs.addAll( wsdlEndpoint.getInboundsConfigs());                    
                   }
                   
               }
           }
    	
    	return inboundsConfigs;
    }
    
    
    public synchronized InboundConfiguration findInboundConfiguration(String listenerName, MediaType contentType, List<MediaType> acceptMediaTypes, String method, String path) {
        for (ServiceUnitConfig suConfig : suConfigs) {
            if (suConfig instanceof PropertyServiceUnitConfig) {
                PropertyServiceUnitConfig propertySUConfig = (PropertyServiceUnitConfig) suConfig;
                if (!propertySUConfig.isOutbound()) {
                    InboundConfiguration inboundConfig = propertySUConfig.getInboundConfiguration();
                    if (inboundConfig.getHttpListenerName().equals(listenerName)) {
                        if (PathUtil.matchInboundConfiguration(inboundConfig, contentType, acceptMediaTypes, method, path)) {
                            return inboundConfig;
                        }
                    }
                }
                
            } else if (suConfig instanceof WSDLEndpoint) {
                WSDLEndpoint wsdlEndpoint = (WSDLEndpoint) suConfig;
                if (!wsdlEndpoint.isOutbound()) {
                    InboundConfiguration inboundConfig = wsdlEndpoint.findInboundConfig(listenerName, contentType, acceptMediaTypes, method, path);
                    if (inboundConfig != null) {
                        return inboundConfig;
                    }
                }
                
            }
        }
        
        return null;
    }
    
    public synchronized Class<?> loadFilterClass(String className) throws Exception {
        if (suClassloader == null) {
            throw new Exception("service unit classload not initialized yet.");
        }
        return Class.forName(className, true, suClassloader);
    }
}


