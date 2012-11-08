/*
 * @(#)NamespaceGenerator.java        $Revision: 1.1 $ $Date: 2008/12/17 23:24:43 $
 * 
 * Copyright (c) 2008 Milan Fort (http://www.milanfort.com/). All rights reserved.
 * 
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the "License"). You may not use this file except
 * in compliance with the License.
 * 
 * You can obtain a copy of the license at http://www.sun.com/cddl/cddl.html.
 * See the License for the specific language governing permissions and limitations
 * under the License.
 */

package org.openesb.components.rules4jbi.shared.classloader;

import java.util.Arrays;
import java.util.List;

/**
 * This class is responsible for generating (somewhat) meaningful namespace strings.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1 $ $Date: 2008/12/17 23:24:43 $
 * 
 * @since 0.4
 */
final class NamespaceGenerator {
    
    private static final String PACKAGE_URI = "urn:package:";
    
    private static final String DEFAULT_PACKAGE_URI = PACKAGE_URI + "default";
    
    /* List of top-level domains. Adapted from http://data.iana.org/TLD/tlds-alpha-by-domain.txt */
    private static final String[] TLDs = {
        "AC", "AD", "AE", "AERO", "AF", "AG", "AI", "AL", "AM", "AN", "AO", "AQ", "AR", "ARPA", "AS", "ASIA",
        "AT", "AU", "AW", "AX", "AZ", "BA", "BB", "BD", "BE", "BF", "BG", "BH", "BI", "BIZ", "BJ", "BM", "BN",
        "BO", "BR", "BS", "BT", "BV", "BW", "BY", "BZ", "CA", "CAT", "CC", "CD", "CF", "CG", "CH", "CI", "CK",
        "CL", "CM", "CN", "CO", "COM", "COOP", "CR", "CU", "CV", "CX", "CY", "CZ", "DE", "DJ", "DK", "DM","DZ",
        "EC", "EDU", "EE", "EG", "ER", "ES", "ET", "EU", "FI", "FJ", "FK", "FM", "FO", "FR", "GA", "GB", "GD",
        "GE", "GF", "GG", "GH", "GI", "GL", "GM", "GN", "GOV", "GP", "GQ", "GR", "GS", "GT", "GU", "GW", "GY",
        "HK", "HM", "HN", "HR", "HT", "HU", "ID", "IE", "IL", "IM", "IN", "INFO", "INT", "IO", "IQ", "IR", "IS",
        "IT", "JE", "JM", "JO", "JOBS", "JP", "KE", "KG", "KH", "KI", "KM", "KN", "KP", "KR", "KW", "KY", "KZ",
        "LA", "LB", "LC", "LI", "LK", "LR", "LS", "LT", "LU", "LV", "LY", "MA", "MC", "MD", "ME", "MG", "MH",
        "MIL", "MK", "ML", "MM", "MN", "MO", "MOBI", "MP", "MQ", "MR", "MS", "MT", "MU", "MUSEUM", "MV", "MW",
        "MX", "MY", "MZ", "NA", "NAME", "NC", "NE", "NET", "NF", "NG", "NI", "NL", "NO", "NP", "NR", "NU", "NZ",
        "OM", "ORG", "PA", "PE", "PF", "PG", "PH", "PK", "PL", "PM", "PN", "PR", "PRO", "PS", "PT", "PW", "PY",
        "QA", "RE", "RO", "RS", "RU", "RW", "SA", "SB", "SC", "SD", "SE", "SG", "SH", "SI", "SJ", "SK", "SL",
        "SM", "SN", "SO", "SR", "ST", "SU", "SV", "SY", "SZ", "TC", "TD", "TEL", "TF", "TG", "TH", "TJ", "TK",
        "TL", "TM", "TN", "TO", "TP", "TR", "TRAVEL", "TT", "TV", "TW", "TZ", "UA", "UG", "UK", "US", "UY",
        "UZ", "VA", "VC", "VE", "VG", "VI", "VN", "VU", "WF", "WS", "YE", "YT", "YU", "ZA", "ZM", "ZW"
    };
    
    private static final List<String> TOP_LEVEL_DOMAINS = Arrays.asList(TLDs);
    
    private NamespaceGenerator() {}
    
    public static String namespaceForClass(final String internalClassName) {
        
        if (internalClassName.indexOf("/") == -1) {
            
            return DEFAULT_PACKAGE_URI;
        }

        final String packageName = internalClassName
                .substring(0, internalClassName.lastIndexOf("/"))
                .replace("/", ".");
        
        if (packageName.indexOf(".") == -1) {
            
            return PACKAGE_URI + packageName;
        }
        
        final String[] packageParts = packageName.split("\\.");
        
        if (TOP_LEVEL_DOMAINS.contains(packageParts[0].toUpperCase())) {
            
            StringBuilder sb = new StringBuilder("http://www.");
            
            sb.append(packageParts[1]);
            sb.append(".");
            sb.append(packageParts[0]);
            sb.append("/");
            
            for (int i = 2; i < packageParts.length; i++) {
                sb.append(packageParts[i]);
                sb.append("/");
            }

            if (packageParts.length > 2) {
                sb.deleteCharAt(sb.length() - 1);
            }
            
            return sb.toString();
            
        } else {
            return PACKAGE_URI + packageName;
        }
    }
}
