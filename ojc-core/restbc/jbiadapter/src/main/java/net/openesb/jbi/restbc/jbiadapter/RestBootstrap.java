package net.openesb.jbi.restbc.jbiadapter;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.jbi.component.Bootstrap;
import javax.jbi.component.InstallationContext;
import javax.jbi.management.DeploymentException;
import javax.jbi.management.MBeanNames;
import javax.management.ObjectName;

import net.openesb.jbi.restbc.jbiadapter.mbeans.RuntimeConfig;

import com.sun.jbi.common.qos.config.ComponentConfig;
import com.sun.jbi.common.qos.config.ConfigPersistence;
import com.sun.jbi.common.util.MBeanHelper;

/**
 * RestBootstrap.java
 *
 * @author Edward Chou
 */
public class RestBootstrap implements Bootstrap {
    
    /*
     * 21-30
     */
    private static final Logger logger = Logger.getLogger(RestBootstrap.class.getName());

    private MBeanHelper mbnHelper;
    private String wspRoot;
    private ComponentConfig ccfg;
    private Object mbean;
    
    public RestBootstrap() {
        
    }

    public void init(InstallationContext ic) throws JBIException {
        if (logger.isLoggable(Level.FINEST)) {
            String msg = I18n.lf("RESTBC-1021: Bootstrap init called.");//NOI18N
            logger.finest(msg);
        }

        try {
            mbnHelper = new MBeanHelper(ic.getContext());
            wspRoot = ic.getContext().getWorkspaceRoot();
            ccfg = ComponentConfig.parse(ic.getInstallRoot());
            mbean = new RuntimeConfig(ic.getContext(), ccfg);
            mbnHelper.registerMBean(MBeanNames.BOOTSTRAP_EXTENSION, mbean, true);
        } catch (DeploymentException de) {
            throw error(de, "RESTBC-7021: Failed to parse component descriptor: {0}", de.getMessage());
        } catch (JBIException je) {
            throw error(je, "RESTBC-7022: Failed to initialize component bootstrap: {0}", je.getMessage());
        } catch (Exception e) {
            throw error(e, "RESTBC-7023: Failed to register component bootstrap: {0}", e.getMessage());
        }
    }

    public void onInstall() throws JBIException {
        ConfigPersistence.persistConfig(ccfg, wspRoot);
        ConfigPersistence.persistApplicationConfig(ccfg, wspRoot);
    }

    public void onUninstall() throws JBIException {
        
    }

    public void cleanUp() throws JBIException {
        if (this.mbnHelper != null) {
            this.mbnHelper.unregisterMBean(MBeanNames.BOOTSTRAP_EXTENSION);
        }
    }
    
    public ObjectName getExtensionMBeanName() {
        return mbnHelper.getObjectName(MBeanNames.BOOTSTRAP_EXTENSION);
    }
    
    private JBIException error(Exception e, String msg, Object... params) {
        String err = I18n.loc(msg, params);
        if (e == null) {
            logger.warning(err);
            return new JBIException(err);
        } else {
            logger.log(Level.WARNING, err, e);
            return new JBIException(err, e);
        }
    }    
}

