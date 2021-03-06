/*
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the License). You may not use this file except in
 * compliance with the License.
 *
 * You can obtain a copy of the License at http://www.netbeans.org/cddl.html
 * or http://www.netbeans.org/cddl.txt.
 * 
 * When distributing Covered Code, include this CDDL Header Notice in each file
 * and include the License file at http://www.netbeans.org/cddl.txt.
 * If applicable, add the following below the CDDL Header, with the fields
 * enclosed by brackets [] replaced by your own identifying information:
 * "Portions Copyrighted [year] [name of copyright owner]"
 *
 * The Original Software is NetBeans. The Initial Developer of the Original
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2006 Sun
 * Microsystems, Inc. All Rights Reserved.
 */
package org.netbeans.modules.jbi.apisupport.project.classpath;

import java.beans.PropertyChangeEvent;
import org.netbeans.modules.jbi.apisupport.project.JbiCompProjectUtil;
import org.netbeans.spi.java.classpath.ClassPathImplementation;
import org.netbeans.spi.java.classpath.support.ClassPathSupport;
import org.netbeans.api.java.platform.JavaPlatform;
import org.netbeans.api.java.platform.JavaPlatformManager;
import org.netbeans.api.java.classpath.ClassPath;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.List;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import org.netbeans.spi.project.support.ant.PropertyEvaluator;
import org.openide.util.WeakListeners;

final class BootClassPathImplementation implements ClassPathImplementation, PropertyChangeListener {

    private static final String PLATFORM_ACTIVE = "platform.active";        //NOI18N
    private static final String ANT_NAME = "platform.ant.name";             //NOI18N
    private static final String J2SE = "j2se";                              //NOI18N

    private final PropertyEvaluator evaluator;
    private JavaPlatformManager platformManager;
    //name of project active platform
    private String activePlatformName;
    //active platform is valid (not broken reference)
    private boolean isActivePlatformValid;
    private List resourcesCache;
    private PropertyChangeSupport support = new PropertyChangeSupport(this);

    public BootClassPathImplementation(PropertyEvaluator evaluator) {
        assert evaluator != null;
        this.evaluator = evaluator;
        evaluator.addPropertyChangeListener(WeakListeners.propertyChange(this, evaluator));
    }

    public synchronized List /*<PathResourceImplementation>*/ getResources() {
        if (this.resourcesCache == null) {
            JavaPlatform jp = findActivePlatform ();
            if (jp != null) {
                //TODO: May also listen on CP, but from Platform it should be fixed.
                ClassPath cp = jp.getBootstrapLibraries();
                List entries = cp.entries();
                ArrayList result = new ArrayList (entries.size());
                for (Iterator it = entries.iterator(); it.hasNext();) {
                    ClassPath.Entry entry = (ClassPath.Entry) it.next();
                    result.add (ClassPathSupport.createResource(entry.getURL()));
                }
                resourcesCache = Collections.unmodifiableList (result);
            }
            else {
                resourcesCache = Collections.EMPTY_LIST;
            }
        }
        return this.resourcesCache;
    }

    public void addPropertyChangeListener(PropertyChangeListener listener) {
        this.support.addPropertyChangeListener (listener);
    }

    public void removePropertyChangeListener(PropertyChangeListener listener) {
        this.support.removePropertyChangeListener (listener);
    }

    private JavaPlatform findActivePlatform () {
        if (this.platformManager == null) {
            this.platformManager = JavaPlatformManager.getDefault();
            this.platformManager.addPropertyChangeListener(WeakListeners.propertyChange(this, this.platformManager));
        }                
        this.activePlatformName = evaluator.getProperty(PLATFORM_ACTIVE);
        final JavaPlatform activePlatform = JbiCompProjectUtil.getActivePlatform (this.activePlatformName);
        this.isActivePlatformValid = activePlatform != null;
        return activePlatform;
    }
    
    public void propertyChange(PropertyChangeEvent evt) {
        if (evt.getSource() == this.evaluator && evt.getPropertyName().equals(PLATFORM_ACTIVE)) {
            //Active platform was changed
            resetCache ();
        }
        else if (evt.getSource() == this.platformManager && JavaPlatformManager.PROP_INSTALLED_PLATFORMS.equals(evt.getPropertyName()) && activePlatformName != null) {
            //Platform definitions were changed, check if the platform was not resolved or deleted
            if (this.isActivePlatformValid) {
                if (JbiCompProjectUtil.getActivePlatform (this.activePlatformName) == null) {
                    //the platform was not removed
                    this.resetCache();
                }
            }
            else {
                if (JbiCompProjectUtil.getActivePlatform (this.activePlatformName) != null) {
                    this.resetCache();
                }
            }
        }
    }
    
    /**
     * Resets the cache and firesPropertyChange
     */
    private void resetCache () {
        synchronized (this) {
            resourcesCache = null;
        }
        support.firePropertyChange(PROP_RESOURCES, null, null);
    }
    
}
