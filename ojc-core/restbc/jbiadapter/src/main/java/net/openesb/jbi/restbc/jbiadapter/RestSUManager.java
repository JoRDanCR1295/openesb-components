package net.openesb.jbi.restbc.jbiadapter;

import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.component.ComponentContext;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.management.DeploymentException;
import javax.ws.rs.core.MediaType;
import javax.xml.namespace.QName;

import net.openesb.jbi.restbc.jbiadapter.inbound.InboundHttpListener;
import net.openesb.jbi.restbc.jbiadapter.inbound.ServiceUnitResourceConfig;

import com.sun.jbi.management.message.DefaultJBITaskMessageBuilder;
import com.sun.jbi.management.message.JBITaskMessageBuilder;
import java.util.Iterator;

/**
 * RestSUManager.java
 *
 * @author Edward Chou
 */
public class RestSUManager implements ServiceUnitManager {

    /*
     * 31-50
     */
    private static final Logger logger = Logger.getLogger(RestSUManager.class.getName());
    private static final String COMP_SUFFIX_NAME = "sun-rest-binding"; //NOI18N
    
    private RestComponent component;
    private ComponentContext context;
    
    // list of ServiceUnits loaded
    private ConcurrentMap<String, ServiceUnit> serviceUnits = new ConcurrentHashMap<String, ServiceUnit> ();
    
    public RestSUManager(RestComponent component, ComponentContext context) {
        this.component = component;
        this.context = context;
    }
    
    /* (non-Javadoc)
     * @see javax.jbi.component.ServiceUnitManager#deploy(java.lang.String, java.lang.String)
     */
    public String deploy(String serviceUnitName, String serviceUnitRootPath) throws DeploymentException {
        if (logger.isLoggable(Level.FINEST)) {
            String msg = I18n.lf("RESTBC-1031: ServiceUnitManager.deploy() called serviceUnitName={0}, serviceUnitRootPath={1}", serviceUnitName, serviceUnitRootPath);//NOI18N
            logger.finest(msg);
        }
        
        String retMsg = null;
        String taskName = "deploy";
                
        retMsg = createSuccessMessage(taskName, context.getComponentName());
        
        String[] m1 = I18n.locStr("RESTBC-5031: Deployed ServiceUnit {0}.", serviceUnitName);
        logger.info(m1[2]);
        I18n.alertInfo(m1);
        
        return retMsg;
    }

    /* (non-Javadoc)
     * @see javax.jbi.component.ServiceUnitManager#init(java.lang.String, java.lang.String)
     */
    public void init(String serviceUnitName, String serviceUnitRootPath) throws DeploymentException {
        if (logger.isLoggable(Level.FINEST)) {
            String msg = I18n.lf("RESTBC-1032: ServiceUnitManager.init() called serviceUnitName={0}, serviceUnitRootPath={1}", serviceUnitName, serviceUnitRootPath);//NOI18N
            logger.finest(msg);
        }
        
        String taskName = "init";
        
        try {
            ServiceUnit su = new ServiceUnit(component, context, serviceUnitName, serviceUnitRootPath);
            ServiceUnit prevValue = serviceUnits.putIfAbsent(su.getServiceUnitName(), su);
            if (prevValue == null) {
                if (logger.isLoggable(Level.FINEST)) {
                    String msg = I18n.lf("RESTBC-1033: ServiceUnitManager.init() successful serviceUnitName={0}", serviceUnitName);//NOI18N
                    logger.finest(msg);
                }
            } else {
                String msg = I18n.loc("RESTBC-7031: Duplicate ServiceUnit {0}, cannot proceed with deployment.", serviceUnitName);//NOI18N
                logger.severe(msg);
                Exception ex = new Exception(msg);
                String exMsg = createExceptionMessage(context.getComponentName(),
                        taskName,
                        "FAILED",
                        "",
                        serviceUnitName,
                        ex.getLocalizedMessage(),
                        ex);
                throw new DeploymentException(exMsg, ex);
            }
        } catch (Exception ex) {
            String exMsg = createExceptionMessage(context.getComponentName(),
                    taskName,
                    "FAILED",
                    "",
                    serviceUnitName,
                    "Processing deployment error: " + ex.getLocalizedMessage(),
                    ex);
            throw new DeploymentException(exMsg, ex);
        }
    }

    /* (non-Javadoc)
     * @see javax.jbi.component.ServiceUnitManager#shutDown(java.lang.String)
     */
    public void shutDown(String serviceUnitName) throws DeploymentException {
        if (logger.isLoggable(Level.FINEST)) {
            String msg = I18n.lf("RESTBC-1034: ServiceUnitManager.shutdown() called serviceUnitName={0}", serviceUnitName);//NOI18N
            logger.finest(msg);
        }
        
        String taskName = "shutDown";
        
        ServiceUnit su = serviceUnits.remove(serviceUnitName);
        if (su != null) {
            try {
                su.shutdown();
            } catch (Exception ex) {
                String msg = I18n.loc("RESTBC-7032: Error shutting down Service Unit {0} {1}", serviceUnitName, ex);//NOI18N
                logger.severe(msg);
                String exMsg =
                        createExceptionMessage(context.getComponentName(),
                        taskName,
                        "FAILED",
                        "",
                        null,
                        msg,
                        ex);
                throw new DeploymentException(exMsg, ex);
            }
        } else {
            String msg = I18n.loc("RESTBC-7033: Cannot find Service Unit {0} to shutdown", serviceUnitName);//NOI18N
            logger.severe(msg);
            Exception ex = new Exception(msg);
            String exMsg = createExceptionMessage(context.getComponentName(),
                    taskName,
                    "FAILED",
                    "",
                    null,
                    ex.getLocalizedMessage(),
                    ex);
            throw new DeploymentException(exMsg);
        }
    }

    /* (non-Javadoc)
     * @see javax.jbi.component.ServiceUnitManager#start(java.lang.String)
     */
    public void start(String serviceUnitName) throws DeploymentException {
        if (logger.isLoggable(Level.FINEST)) {
            String msg = I18n.lf("RESTBC-1035: ServiceUnitManager.start() called serviceUnitName={0}", serviceUnitName);//NOI18N
            logger.finest(msg);
        }
        
        String taskName = "start";
        ServiceUnit su = serviceUnits.get(serviceUnitName);
        if (su != null) {
            try {
                Iterator<InboundConfiguration> ite = su.getInboundsConfigurations().iterator();
                if (ite.hasNext()) {
                    String listenerName = ite.next().getHttpListenerName();
                    InboundHttpListener inboundHttpListener = component.getInboundHttpListener(listenerName);
                    inboundHttpListener.registerContext(getSUContextPath(su), new ServiceUnitResourceConfig(su));

                    su.start();
                }
            } catch (Exception ex) {
                String msg = I18n.loc("RESTBC-7034: Error starting Service Unit {0} {1}", serviceUnitName, ex);//NOI18N
                logger.severe(msg);
                String exMsg = createExceptionMessage(context.getComponentName(),
                        taskName,
                        "FAILED",
                        "",
                        null,
                        msg,
                        ex);
                throw new DeploymentException(exMsg, ex);
            }
        } else {
            String msg = I18n.loc("RESTBC-7035: Cannot find Service Unit {0} to start", serviceUnitName);//NOI18N
            logger.severe(msg);
            Exception ex = new Exception(msg);
            String exMsg = createExceptionMessage(context.getComponentName(),
                    taskName,
                    "FAILED",
                    "",
                    null,
                    ex.getLocalizedMessage(),
                    ex);
            throw new DeploymentException(exMsg);
        }
        
    }
    /**
     * Give the Service Unit context path.
     *  
     * @param su Service Unit
     * @return  Service unit context path
     */
    protected String getSUContextPath(ServiceUnit su){
    	return "/"+su.getServiceUnitName().substring(0, su.getServiceUnitName().length()-(COMP_SUFFIX_NAME.length()+1));
    }

    /* (non-Javadoc)
     * @see javax.jbi.component.ServiceUnitManager#stop(java.lang.String)
     */
    public void stop(String serviceUnitName) throws DeploymentException {
        if (logger.isLoggable(Level.FINEST)) {
            String msg = I18n.lf("RESTBC-1036: ServiceUnitManager.stop() called serviceUnitName={0}", serviceUnitName);//NOI18N
            logger.finest(msg);
        }
        
        String taskName = "stop";
       
        ServiceUnit su = serviceUnits.get(serviceUnitName);
        if (su != null) {
            try {
                Iterator<InboundConfiguration> ite = su.getInboundsConfigurations().iterator();
                if (ite.hasNext()) {
                    String listenerName = ite.next().getHttpListenerName();
                    InboundHttpListener inboundHttpListener = component.getInboundHttpListener(listenerName);
                    inboundHttpListener.unregisterContext(getSUContextPath(su));
                    
                    su.stop();
                }
            } catch (Exception ex) {
                String msg = I18n.loc("RESTBC-7036: Error stopping Service Unit {0} {1}", serviceUnitName, ex);//NOI18N
                logger.severe(msg);
                String exMsg =
                        createExceptionMessage(context.getComponentName(),
                        taskName,
                        "FAILED",
                        "",
                        null,
                        msg,
                        ex);
                throw new DeploymentException(exMsg, ex);
            }
        } else {
            String msg = I18n.loc("RESTBC-7037: Cannot find Service Unit {0} to stop", serviceUnitName);//NOI18N
            logger.severe(msg);
            Exception ex = new Exception(msg);
            String exMsg = createExceptionMessage(context.getComponentName(),
                    taskName,
                    "FAILED",
                    "",
                    null,
                    ex.getLocalizedMessage(),
                    ex);
            throw new DeploymentException(exMsg);
        }
        
    }

    /* (non-Javadoc)
     * @see javax.jbi.component.ServiceUnitManager#undeploy(java.lang.String, java.lang.String)
     */
    public String undeploy(String serviceUnitName, String serviceUnitRootPath) throws DeploymentException {
        if (logger.isLoggable(Level.FINEST)) {
            String msg = I18n.lf("RESTBC-1037: ServiceUnitManager.undeploy() called serviceUnitName={0}, serviceUnitRootPath={1}", serviceUnitName, serviceUnitRootPath);//NOI18N
            logger.finest(msg);
        }
        
        String taskName = "undeploy";
        
        return createSuccessMessage(taskName, context.getComponentName());
    }
    
    public OutboundConfiguration findActivatedEndpointOutbound(QName serviceName, String endpointName, QName operationName) {
        for (ServiceUnit su : serviceUnits.values()) {
            OutboundConfiguration outboundConfig = su.findActivatedEndpointOutbound(serviceName, endpointName, operationName);
            if (outboundConfig != null) {
                return outboundConfig;
            }
        }
        return null;
    }
    

    
    public InboundConfiguration findInboundConfiguration(String listenerName, MediaType contentType, List<MediaType> acceptMediaTypes, String method, String path) {
        for (ServiceUnit su : serviceUnits.values()) {
            InboundConfiguration inboundConfig = su.findInboundConfiguration(listenerName, contentType, acceptMediaTypes, method, path);
            if (inboundConfig != null) {
                return inboundConfig;
            }
        }
        return null;
    }
    
    
    
    private String createSuccessMessage(String taskName, String componentName) {
        JBITaskMessageBuilder msgBuilder = new DefaultJBITaskMessageBuilder();
        msgBuilder.setComponentName(componentName);
        String retMsg = msgBuilder.createSuccessMessage(taskName);
        return retMsg;
    }
    
    private String createExceptionMessage(String componentName,
                                          String taskName,
                                          String status,
                                          String locToken,
                                          String locParam,
                                          String locMessage,
                                          Throwable exObj) {
        JBITaskMessageBuilder msgBuilder = new DefaultJBITaskMessageBuilder();
        msgBuilder.setComponentName(componentName);
        String retMsg = msgBuilder.createExceptionMessage(taskName, locToken, locMessage, locParam, exObj);
        return retMsg;
    }
    
}
