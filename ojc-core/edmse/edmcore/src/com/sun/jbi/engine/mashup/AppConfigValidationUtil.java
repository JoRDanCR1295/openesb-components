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
import com.sun.jbi.internationalization.Messages;
import java.util.Map;

/**
 * @author Sun Microsystems
 */
public class AppConfigValidationUtil {
    
    private static final Messages mMessages = Messages.getMessages(AppConfigValidationUtil.class);

    public static void validateApplicationConfigName(MashupSERuntimeConfigurationMBean configMBean, String configName) throws EDMApplicationException {
        Map appconfigMap = configMBean.retrieveApplicationConfigurationsMap();
        if (appconfigMap.get(configName) == null) {
            throw new EDMApplicationException(mMessages.getString("EDMSE-E0100.Invalid_AppConfig_Name"));
        }
    }
}
