/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openesb.components.camelse.nb.plugin;

import java.util.prefs.Preferences;
import org.openide.util.NbPreferences;

/**
 *
 * @author chikkala
 */
public class MyPreferences {
    
    private static final String CAMEL_HOME = "CamelHome";
    private static final String DEF_CAMEL_HOME = "/apache-camel-1.5.0";
    private static Preferences getPreferences() {
        return NbPreferences.forModule(MyPreferences.class);
    }
    /**
     * Get the value of camelHome
     *
     * @return the value of camelHome
     */
    public static String getCamelHome() {
        return getPreferences().get(CAMEL_HOME, DEF_CAMEL_HOME);
    }

    /**
     * Set the value of camelHome
     *
     * @param camelHome new value of camelHome
     */
    public static void setCamelHome(String camelHome) {
        getPreferences().put(CAMEL_HOME, camelHome);
    }
 
}
