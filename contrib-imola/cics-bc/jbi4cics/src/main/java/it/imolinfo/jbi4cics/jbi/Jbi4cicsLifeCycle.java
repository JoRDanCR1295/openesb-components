/*
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 */


package it.imolinfo.jbi4cics.jbi;

import it.imolinfo.jbi4cics.webservices.runtime.ServiceCreator;
import org.apache.servicemix.common.BaseComponent;
import org.apache.servicemix.common.BaseLifeCycle;
import org.codehaus.xfire.XFire;

/**
 * Class for life cycle management of Jbi4Cics components.
 */
public final class Jbi4cicsLifeCycle extends BaseLifeCycle {

    private Jbi4cicsComponentConfiguration configuration
            = new Jbi4cicsComponentConfiguration();

    /**
     * The XFire instance.
     */
    private XFire xfire;

    /**
     * The byte code engineering library class loader used.
     */
    private BCELClassLoader bcelClassLoader
            = new BCELClassLoader(getClass().getClassLoader());

    /**
     * Initializes the life cycle instance for the specified ServiceMix
     * component.
     *
     * @param  component  the ServiceMix component managed by this instance.
     */
    public Jbi4cicsLifeCycle(BaseComponent component) {
        super(component);
    }

    /**
     * Returns the XFire instance.
     *
     * @return  the XFire instance.
     */
    public XFire getXFire() {
        return xfire;
    }

    @Override
    protected void doInit() throws Exception {
        super.doInit();
        configuration.setRootDir(context.getWorkspaceRoot());
        configuration.load();
        xfire = ServiceCreator.createXFire(context);
    }

    /**
     * Returns the byte code engineering library class loader.
     *
     * @return  the byte code engineering library class loader.
     */
    public BCELClassLoader getBCELClassLoader() {
        return bcelClassLoader;
    }

    /**
     * Sets the byte code engineering library class loader.
     *
     * @param  bcelClassLoader  the byte code engineering library class loader.
     */
    public void setBCELClassLoader(BCELClassLoader bcelClassLoader) {
        this.bcelClassLoader = bcelClassLoader;
    }
}
