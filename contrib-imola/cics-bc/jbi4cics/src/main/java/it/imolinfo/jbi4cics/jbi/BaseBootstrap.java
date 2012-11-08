/*
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 */


package it.imolinfo.jbi4cics.jbi;

import it.imolinfo.jbi4cics.Logger;
import it.imolinfo.jbi4cics.LoggerFactory;

import javax.jbi.JBIException;
import javax.jbi.component.Bootstrap;
import javax.jbi.component.InstallationContext;
import javax.jbi.management.MBeanNames;
import javax.management.MBeanServer;
import javax.management.ObjectName;

/**
 * Base class for components bootstrap. Due to classloading mechanism in JBI,
 * Shared Libraries are not available at bootstrap time, so this class should be
 * copied in your own component and modified directly, instead of inheriting it.
 *
 * @author  Guillaume Nodet
 * @author  <a href="mailto:mcimatti@imolinfo.it">Marco Cimatti</a>
 * @version $Revision: 1.4 $
 * @since   3.0
 */
public class BaseBootstrap implements Bootstrap {

    /**
     * The logger for this class and its instances.
     */
    private static final Logger LOG
            = LoggerFactory.getLogger(BaseBootstrap.class);

    /**
     * The responsible to translate localized messages.
     */
    private static final Messages MESSAGES
            = Messages.getMessages(BaseBootstrap.class);

    /**
     * The context containing information from the install command and from the
     * component installation ZIP file.
     *
     * @see  #init(InstallationContext)
     */
    private InstallationContext context;

    /**
     * The <code>ObjectName</code> of the installer configuration MBean.
     *
     * @see  #getExtensionMBeanName()
     */
    private ObjectName mbeanName;

    /**
     * Creates a new instance of this class. This constructor is required by JBI
     * specifications.
     */
    public BaseBootstrap() {
    }

    /**
     * Obtains the <code>ObjectName</code> of the optional installer
     * configuration MBean. If none is provided by this component, this method
     * must return <code>null</code>.
     * <p>
     * This method must be called before <code>onInstall()</code> (or
     * <code>onUninstall()</code>) is called by the JBI implementation.
     *
     * @return  the <code>ObjectName</code> of the optional installer
     *          configuration MBean; returns <code>null</code> if there is no
     *          such MBean.
     */
    public final ObjectName getExtensionMBeanName() {
        return mbeanName;
    }

    protected Object getExtensionMBean() throws Exception {
        return null;
    }

    protected ObjectName createExtensionMBeanName() throws Exception {
        MBeanNames names = context.getContext().getMBeanNames();

        return names.createCustomComponentMBeanName("bootstrap");
    }

    /**
     * Initializes the installation environment for a component. This method is
     * expected to save any information from the installation context that may
     * be needed by other methods.
     * <p>
     * If the component needs to register an optional installer configuration
     * MBean, it MUST do so during execution of this method, or the
     * <code>getExtensionMBean()</code> method.
     * <p>
     * This method must be called after the installation root (available through
     * the <code>installContext</code> parameter) is prepared.
     *
     * @param   installContext  the context containing information from the
     *                          install command and from the component
     *                          installation ZIP file; this must be
     *                          non-<code>null</code>.
     * @throws  JBIException    when there is an error requiring that the
     *                          installation be terminated.
     */
    public final void init(final InstallationContext installContext)
            throws JBIException {
        try {
            LOG.debug("Initializing bootstrap");
            context = installContext;
            doInit();
            LOG.debug("Bootstrap initialized");
        } catch (JBIException e) {
            LOG.error(e.getLocalizedMessage(), e);
            throw e;
        } catch (Exception e) {
            Object[] args = new Object[] { e.getLocalizedMessage() };

            LOG.error("CIC001005_Init_error", args, e);
            throw new JBIException(
                    MESSAGES.getString("CIC001005_Init_error", args), e);
        }
    }

    protected void doInit() throws Exception {
        Object mbean = getExtensionMBean();

        if (mbean != null) {
            MBeanServer server;

            mbeanName = createExtensionMBeanName();
            server = context.getContext().getMBeanServer();
            if (server == null) {
                throw new JBIException(
                        MESSAGES.getString("CIC001006_Null_mBeanServer"));
            }
            if (server.isRegistered(mbeanName)) {
                server.unregisterMBean(mbeanName);
            }
            server.registerMBean(mbean, mbeanName);
        }
    }

    /**
     * Cleans up any resources allocated by the bootstrap implementation,
     * including performing deregistration of the extension MBean, if
     * applicable.
     * <p>
     * This method must be called after the <code>onInstall()</code> or
     * <code>onUninstall()</code> method is called, whether it succeeds or
     * fails. It must be called after <code>init()</code> is called, if
     * <code>init()</code> fails by throwing an exception.
     *
     * @throws  JBIException  if the bootstrap cannot clean up allocated
     *                        resources.
     */
    public final void cleanUp() throws JBIException {
        try {
            LOG.debug("Cleaning up bootstrap");
            doCleanUp();
            LOG.debug("Bootstrap cleaned up");
        } catch (JBIException e) {
            LOG.error(e.getLocalizedMessage(), e);
            throw e;
        } catch (Exception e) {
            Object[] args = new Object[] { e.getLocalizedMessage() };

            LOG.error("CIC001007_Clean_up_error", args, e);
            throw new JBIException(
                    MESSAGES.getString("CIC001007_Clean_up_error", args), e);
        }
    }

    protected void doCleanUp() throws Exception {
        if (mbeanName != null) {
            MBeanServer server = context.getContext().getMBeanServer();

            if (server == null) {
                throw new JBIException(
                        MESSAGES.getString("CIC001006_Null_mBeanServer"));
            }
            if (server.isRegistered(mbeanName)) {
                server.unregisterMBean(mbeanName);
            }
        }
    }

    /**
     * Called at the beginning of installation of a component to perform any
     * special installation tasks required by the component.
     * <p>
     * This method must not be called if the <code>init()</code> method failed
     * with an exception.
     *
     * @throws  JBIException  when there is an error requiring that the
     *                        installation be terminated.
     */
    public final void onInstall() throws JBIException {
        try {
            LOG.debug("Bootstrap onInstall");
            doOnInstall();
            LOG.debug("Bootstrap onInstall done");
        } catch (JBIException e) {
            LOG.error(e.getLocalizedMessage(), e);
            throw e;
        } catch (Exception e) {
            Object[] args = new Object[] { e.getLocalizedMessage() };

            LOG.error("CIC001008_On_install_error", args, e);
            throw new JBIException(
                    MESSAGES.getString("CIC001008_On_install_error", args), e);
        }
    }

    protected void doOnInstall() throws Exception {
    }

    /**
     * Called at the beginning of uninstallation of a component to perform any
     * special uninstallation tasks required by the component.
     * <p>
     * This method must not be called if the <code>init()</code> method failed
     * with an exception.
     *
     * @throws  JBIException  when there is an error requiring that the
     *                        uninstallation be terminated.
     */
    public final void onUninstall() throws JBIException {
        try {
            LOG.debug("Bootstrap onUninstall");
            doOnUninstall();
            LOG.debug("Bootstrap onUninstall done");
        } catch (JBIException e) {
            LOG.error(e.getLocalizedMessage(), e);
            throw e;
        } catch (Exception e) {
            Object[] args = new Object[] { e.getLocalizedMessage() };

            LOG.error("CIC001009_On_uninstall_error", args, e);
            throw new JBIException(MESSAGES.getString(
                    "CIC001009_On_uninstall_error", args), e);
        }
    }

    protected void doOnUninstall() throws Exception {
    }
}
