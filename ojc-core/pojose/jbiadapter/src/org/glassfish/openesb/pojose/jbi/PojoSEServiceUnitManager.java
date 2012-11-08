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
 * @(#)PojoSEServiceUnitManager.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.pojose.jbi;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.util.ManagementMessage;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.JBIException;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.management.DeploymentException;
import javax.jbi.messaging.MessagingException;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;
import org.glassfish.openesb.pojose.api.annotation.ConsumerEndpoint;
import org.glassfish.openesb.pojose.api.annotation.Endpoint;
import org.glassfish.openesb.pojose.core.anno.meta.POJOClassMetadata;
import org.glassfish.openesb.pojose.core.anno.processor.Message;
import org.glassfish.openesb.pojose.core.util.Constants;
import org.glassfish.openesb.pojose.jbi.su.PojoSEServiceUnit;
import org.glassfish.openesb.pojose.jbi.su.PojoSEServiceUnit.Status;
import org.glassfish.openesb.pojose.jbi.su.ServiceUnitManagerListener;
import org.glassfish.openesb.pojose.res.impl.ConsumerImpl;
import org.glassfish.openesb.pojose.res.impl.ServiceEndpointImpl;

/**
 *
 * @author gpatil
 */
public class PojoSEServiceUnitManager implements ServiceUnitManager {

    private final POJOComponentContext compCtx;
    private final Logger logger = Logger.getLogger(PojoSEServiceUnitManager.class.getName());
    private final List<ServiceUnitManagerListener> listeners = new Vector<ServiceUnitManagerListener>();
    
    /**
     * @param compCtx
     */
    public PojoSEServiceUnitManager(POJOComponentContext compCtx){
        this.compCtx = compCtx;
    }

    // ***
    // * ServiceUnitManager API methods
    // ***
    public synchronized String deploy(String suName, String suRootPath) throws DeploymentException {
        if (logger.isLoggable(Level.FINE)) {
            String m = I18n.lf("POJOSE-1501: Deploying SU {0} with root {1}", suName, suRootPath);//NOI18N
            logger.fine(m);
        }
        // TODO scan SU info here and persist using compCtx info.
        PojoSEServiceUnit su = new PojoSEServiceUnit(suName, suRootPath);
        if (su.hasDeployErrors()){
            StringBuilder sb = new StringBuilder();
            String m = I18n.loc("POJOSE-7504: Can not deploy SU {0} with below erros.", suName);
            sb.append(m);
            logger.severe(m);
            List<Message> errors = su.getErrorMessages();
            for (Message msg : errors){
                logger.severe(msg.getMessage());
                sb.append("\n"); //NOI18N
                sb.append(msg.getMessage());
            }

            String msg = ManagementMessage.createDeployMessage(
                    compCtx.getJBIComponentContext().getComponentName(),
                    I18n.loc("POJOSE-4000: Deploy"),
                    ManagementMessage.STATUS_FAILED,
                    ManagementMessage.TYPE_ERROR,
                    I18n.loc("POJOSE-4001: POJOSE"),
                    sb.toString(), null, null);

            DeploymentException de = new DeploymentException(msg);
            throw de;
        } else {
            su.setStatus(Status.deployed);
            compCtx.putPojoSU(suName, su);
            updateListeners(su);

            String[] m1 = I18n.locStr("POJOSE-5507: Deployed SU {0}.", suName);
            logger.info(m1[2]);
            I18n.alertInfo(m1);
            return ManagementMessage.createDeployMessage(
                    compCtx.getJBIComponentContext().getComponentName(),
                    I18n.loc("POJOSE-4000: Deploy"),
                    ManagementMessage.STATUS_SUCCESS,
                    ManagementMessage.TYPE_INFO,
                    I18n.loc("POJOSE-4001: POJOSE"),
                    m1[2], null, null);
        }
    }

    public synchronized void init(String suName, String suRootPath) throws DeploymentException {
        if (logger.isLoggable(Level.FINE)) {
            String m = I18n.lf("POJOSE-1502: Initiliazing SU {0} with root {1}", suName, suRootPath);//NOI18N
            logger.fine(m);
        }

        PojoSEServiceUnit su = compCtx.getPojoSU(suName);
        if (su == null){
             su = new PojoSEServiceUnit(suName, suRootPath);
             compCtx.putPojoSU(suName, su);
        }

        assert su != null;

        List<EndpointInfo> eps = su.getProviderEps();
        if (eps != null){
            for (EndpointInfo ei: eps){
                assert ei != null;
                try {
                    ServiceEndpoint sept = this.compCtx.getJBIComponentContext().activateEndpoint(ei.getServiceName(), ei.getEndpointName());

                    String m = I18n.loc("POJOSE-4513: Activated Provisioning endpoint: {0}.", sept);
                    logger.info( m);

                    su.addServiceEndpoint(ei, sept);
                    compCtx.putPojoSU(sept, su);
                } catch (JBIException ex) {
                    String m = I18n.loc("POJOSE-7502: Exception while activating endpoint {0}.", ei);
                    logger.log(Level.SEVERE, m, ex);
                    String msg = ManagementMessage.createDeployMessage(
                            compCtx.getJBIComponentContext().getComponentName(),
                            I18n.loc("POJOSE-4002: Init"),
                            ManagementMessage.STATUS_FAILED,
                            ManagementMessage.TYPE_ERROR,
                            I18n.loc("POJOSE-4001: POJOSE"),
                            m, null, null);

                    throw new DeploymentException(msg, ex);
                }
            }
        }

        // Put consuming endpoint in the POJO Metadata class
        List<POJOClassMetadata> pjs = su.getPojoClassMetadatas();
        for (POJOClassMetadata pm: pjs){
            processConsumerEndpoints(pm);
            processEndpoints(pm);
        }

        try {
            this.compCtx.getDC().installServiceQualities(suName, suRootPath);
        } catch (MessagingException ex) {
            Logger.getLogger(PojoSEServiceUnitManager.class.getName()).log(Level.SEVERE, null, ex);
        }
        su.setStatus(Status.initialized);
        updateListeners(su);

        String m = I18n.loc("POJOSE-4503: Initiliazed SU {0}.", suName);
        logger.info( m);
    }

    private void processConsumerEndpoints(POJOClassMetadata pm) throws DeploymentException {
        Map<String, ConsumerEndpoint> f2e = pm.getField2ConsumerEndpointMap();
        String m = null;
        Iterator<String> keys = f2e.keySet().iterator();
        String fname;
        ConsumerEndpoint ep = null;
        QName svc = null;
        String epn = null;

        ServiceEndpoint se = null;
        ConsumerImpl consImpl = null;

        while (keys.hasNext()) {
            svc = null;
            se = null;

            fname = keys.next();
            ep = f2e.get(fname);
            epn = ep.name();
            svc = QName.valueOf(ep.serviceQN());

            if (epn == null) {
                m = I18n.loc("POJOSE-7506: Endpoint attribute for ConsumerEndpoint annotation can not be empty: {0}.", fname);
                logger.severe(m);
                String msg = ManagementMessage.createDeployMessage(
                        compCtx.getJBIComponentContext().getComponentName(),
                        I18n.loc("POJOSE-4002: Init"),
                        ManagementMessage.STATUS_FAILED,
                        ManagementMessage.TYPE_ERROR,
                        I18n.loc("POJOSE-4001: POJOSE"),
                        m, null, null);

                throw new DeploymentException(msg);
            }

            se = this.compCtx.getJBIComponentContext().getEndpoint(svc, epn);

            if (se != null) {
                m = I18n.loc("POJOSE-4518: Found activated consuming endpoint: {0}.", se);
                logger.info(m);
                try {
                    consImpl = new ConsumerImpl(this.compCtx.getDC(),
                            this.compCtx.getJBIComponentContext(),
                            null);

                    consImpl.setServiceEndpoint(se);

                    if (!Constants.ANNOTATION_NULL_VAL.equals(ep.inMessageTypeQN())){
                        QName inpt = QName.valueOf(ep.inMessageTypeQN());
                        consImpl.setDefaultInputMessageType(inpt);
                    }

                    if (!Constants.ANNOTATION_NULL_VAL.equals(ep.operationQN())){
                        QName opr = QName.valueOf(ep.operationQN());
                        consImpl.setDefaultOperationName(opr);
                    }

                    pm.addConsumer(fname, consImpl);
                } catch (MessagingException me){
                    String msg = ManagementMessage.createDeployMessage(
                            compCtx.getJBIComponentContext().getComponentName(),
                            I18n.loc("POJOSE-4002: Init"),
                            ManagementMessage.STATUS_FAILED,
                            ManagementMessage.TYPE_ERROR,
                            I18n.loc("POJOSE-4001: POJOSE"),
                            me.getMessage(), null, null);

                    throw new DeploymentException(msg, me);
                }
            } else {
                m = I18n.loc("POJOSE-7505: Activated consuming endpoint: {0} not found for field {1}.", //NOI18N
                        svc.toString() + ":" + epn , fname); //NOI18N
                logger.severe(m);
                String msg = ManagementMessage.createDeployMessage(
                        compCtx.getJBIComponentContext().getComponentName(),
                        I18n.loc("POJOSE-4002: Init"),
                        ManagementMessage.STATUS_FAILED,
                        ManagementMessage.TYPE_ERROR,
                        I18n.loc("POJOSE-4001: POJOSE"),
                        m, null, null);

                throw new DeploymentException(msg);
            }
        }
    }

    private void processEndpoints(POJOClassMetadata pm) throws DeploymentException {
        Map<String, Endpoint> f2e = pm.getField2EndpointMap();
        String m = null;
        Iterator<String> keys = f2e.keySet().iterator();
        String fname;
        Endpoint ep = null;
        String epn = null;
        String sn = null;
        String sns = null;
        String in = null;
        String ins = null;
        QName svc = null;
        ServiceEndpoint se = null;
        ServiceEndpointImpl seImpl = null;

        while (keys.hasNext()) {
            svc = null;
            se = null;

            fname = keys.next();
            ep = f2e.get(fname);
            epn = ep.name();
            sn = ep.serviceName();
            if ((sn == null) || ("".equals(sn))) {
                sn = "";
            }
            sns = ep.serviceNS();
            if ((sns == null) || ("".equals(sns))) {
                sns = "";
            }

            if (!Constants.ANNOTATION_NULL_VAL.equals(ep.serviceQN())) {
                svc = QName.valueOf(ep.serviceQN());
            } else {
                svc = new QName(sns, sn);
            }

            in = ep.interfaceName();

            ins = ep.interfaceNS();
            if ((ins == null) || ("".equals(ins))) {
                ins = "";
            }
            if ((in == null) || ("".equals(in))) {
                in = "";
            }

            se = this.compCtx.getJBIComponentContext().getEndpoint(svc, epn);
            if (se != null) {
                m = I18n.loc("POJOSE-4518: Found activated consuming endpoint: {0}.", se);
                logger.info(m);
                seImpl = new ServiceEndpointImpl(se, ep);
                pm.addServiceEndpoint(fname, seImpl);
            } else {
                m = I18n.loc("POJOSE-7505: Activated consuming endpoint: {0} not found for field {1}.", //NOI18N
                        svc.toString() + ":" + epn , fname); //NOI18N
                logger.severe(m);
                String msg = ManagementMessage.createDeployMessage(
                        compCtx.getJBIComponentContext().getComponentName(),
                        I18n.loc("POJOSE-4002: Init"),
                        ManagementMessage.STATUS_FAILED,
                        ManagementMessage.TYPE_ERROR,
                        I18n.loc("POJOSE-4001: POJOSE"),
                        m, null, null);

                throw new DeploymentException(msg);
            }
        }
    }

    public synchronized void start(String suName) throws DeploymentException {
        if (logger.isLoggable(Level.FINE)) {
            String m = I18n.lf("POJOSE-1503: Starting SU {0}.", suName);//NOI18N
            logger.fine(m);
        }

        PojoSEServiceUnit su = compCtx.getPojoSU(suName);
        su.setStatus(Status.started);
        updateListeners(su);

        String[] m1 = I18n.locStr("POJOSE-5508: Started SU {0}", suName);
        logger.info( m1[2]);
        I18n.alertInfo(m1);
    }

    public synchronized void stop(String suName) throws DeploymentException {
        if (logger.isLoggable(Level.FINE)) {
            String m = I18n.lf("POJOSE-1504: Stopping SU {0}.", suName);//NOI18N
            logger.fine(m);
        }

        PojoSEServiceUnit su = compCtx.getPojoSU(suName);
        su.setStatus(Status.stopped);
        updateListeners(su);

        String[] m1 = I18n.locStr("POJOSE-5509: Stopped SU {0}", suName);
        logger.info( m1[2]);
        I18n.alertInfo(m1);
    }

    public synchronized void shutDown(String suName) throws DeploymentException {
        if (logger.isLoggable(Level.FINE)) {
            String m = I18n.lf("POJOSE-1505: Shutting down SU {0}.", suName);//NOI18N
            logger.fine(m);
        }

        PojoSEServiceUnit su = compCtx.getPojoSU(suName);
        if (su != null){
            List<EndpointInfo> eps = su.getProviderEps();
            ServiceEndpoint sept = null;
            if (eps != null){
                for (EndpointInfo ei: eps){
                    try {
                        sept = su.getServiceEndpoint(ei);
                        if (sept != null){
                            this.compCtx.getJBIComponentContext().deactivateEndpoint(sept);
                            compCtx.removePojoSU(sept);
                            if (logger.isLoggable(Level.FINE)) {
                                String m = I18n.lf("POJOSE-1506: Deactivated Provisioning endpoint: {0}.", sept);
                                logger.fine( m);
                            }
                        }
                    } catch (JBIException ex) {
                        String m = I18n.loc("POJOSE-7503: Error deactivating Provisioning endpoint: {0}.", ei);
                        logger.log(Level.SEVERE, m, ex);
                    }
                }
            }
        }

        su.setStatus(Status.shutdown);
        updateListeners(su);

        String[] m1 = I18n.locStr("POJOSE-5510: Shut down SU {0}", suName);
        logger.info( m1[2]);
        I18n.alertInfo(m1);
    }

    public synchronized String undeploy(String suName, String suRootPath) throws DeploymentException {
        if (logger.isLoggable(Level.FINE)) {
            String m = I18n.lf("POJOSE-1507: Undeploying SU {0} with root {1}", suName, suRootPath);//NOI18N
            logger.fine(m);
        }

        PojoSEServiceUnit su = compCtx.getPojoSU(suName);
        su.setStatus(Status.undeployed);
        updateListeners(su);
        try {
            this.compCtx.getDC().uninstallServiceQualities(suName);
        } catch (MessagingException ex) {
            Logger.getLogger(PojoSEServiceUnitManager.class.getName()).log(Level.SEVERE, null, ex);
        }

        String[] m1 = I18n.locStr("POJOSE-5511: Undeployed SU {0}", suName);
        logger.info( m1[2]);
        I18n.alertInfo(m1);
        return ManagementMessage.createDeployMessage(
                compCtx.getJBIComponentContext().getComponentName(),
                I18n.loc("POJOSE-4003: Undeploy"),
                ManagementMessage.STATUS_SUCCESS,
                ManagementMessage.TYPE_INFO,
                I18n.loc("POJOSE-4001: POJOSE"),
                m1[2], null, null);
    }

    public void addListener(ServiceUnitManagerListener l){
        synchronized(this.listeners){
            this.listeners.add(l);
        }
    }

    public void removeListener(ServiceUnitManagerListener l){
        synchronized(this.listeners){
            this.listeners.remove(l);
        }
    }

    private void updateListeners(PojoSEServiceUnit su){
        synchronized (this.listeners) {
            for (ServiceUnitManagerListener l : this.listeners) {
                try {
                    switch (su.getStatus()) {
                        case deployed:
                            l.suDeployed(su);
                            break;
                        case initialized:
                            l.suInitialized(su);
                            break;
                        case started:
                            l.suStarted(su);
                            break;
                        case stopped:
                            l.suStopped(su);
                            break;
                        case shutdown:
                            l.suShutdown(su);
                            break;
                        case undeployed:
                            l.suUndeployed(su);
                            break;
                    }
                } catch (Exception ex) {
                    String msg = I18n.loc("POJOSE-7501: Exception while propagating SU status. {0}", ex);
                    logger.log(Level.SEVERE, msg);
                }
            }
        }
    }
}
