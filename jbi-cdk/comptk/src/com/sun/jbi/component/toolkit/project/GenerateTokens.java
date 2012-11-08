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
 * @(#)GenerateTokens.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.project;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import org.w3c.dom.Element;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.component.toolkit.project.scrub.Profile;
import com.sun.jbi.component.toolkit.project.scrub.ScrubExpr;
import com.sun.jbi.component.toolkit.project.scrub.Scrubber;
import com.sun.jbi.component.toolkit.project.util.I18n;
import com.sun.jbi.component.toolkit.project.util.ProjectException;
import com.sun.jbi.component.toolkit.project.util.XPathElement;
import com.sun.jbi.component.toolkit.project.view.wizard.CreateWizard.CompType;

/**
 * 
 * @author Kevan Simpson
 */
public class GenerateTokens {
    public enum Tkn { 
        CLASS_PREFIX("The value to prepend to generate Java class names") {
            //@=").append(prefix).append("\n\n")    // XquerySE
            public String defaultValue(String projName, CompType type) {
                return PROJ_SHORT_NAME.defaultValue(projName, type) + type.getSuffix();
            }
        },
        COMP_TYPE_FLAG("Must be 'binding' or 'engine'") {//@=").append(mCompType).append("\n\n")    // engine
            public String defaultValue(String projName, CompType type) {
                return type.getType();
            }
        },
        PKG("The package into which source files are generated") {
            //@=").append("com.sun.jbi.").append(mCompType).append(".").append(shortName.toLowerCase()).append("\n\n")    // pkg
            public String defaultValue(String projName, CompType type) {
                StringBuffer buff = new StringBuffer();
                buff.append("com.sun.jbi.")
                    .append(type.getType())
                    .append(".")
                    .append(PROJ_SHORT_LC.defaultValue(projName, type));
                return buff.toString();
            }
        },
        PROJ_NAME("The same value passed in as -Dproject.name") {//@=").append(mProjName).append("\n\n")     // xqueryse
            public String defaultValue(String projName, CompType type) {
                return projName;
            }
        },
        PROJ_ROOT("The root directory of the JBI Component project") {              //  $JV_SRCROOT/ojc-core/xqueryse
            public String defaultValue(String projName, CompType type) {
                File ojcCore = new File(new File(
                        System.getProperty("JV_SRCROOT")), "ojc-core");
                File root = new File(ojcCore, projName);
                return root.getAbsolutePath().replace("\\", "\\\\"); 
            }
        },
        PROJ_SHORT_NAME("Project name in camelcase with the conventional suffix removed ('bc' | 'se')") {//@=").append(shortName).append("\n\n")  //  Xquery
            public String defaultValue(String projName, CompType type) {
                String two = projName.substring(projName.length() - 2);
                String shortNm = (two.equalsIgnoreCase(type.getSuffix()))  // trim suffix
                        ? projName.substring(0, projName.indexOf(two)) : projName;
                return camelCase(shortNm);
            }
        },
        PROJ_SHORT_LC("Project name in lower-case with the conventional suffix removed ('bc' | 'se')") {//@=").append(shortName.toLowerCase()).append("\n\n")  //  xquery
            public String defaultValue(String projName, CompType type) {
                return PROJ_SHORT_NAME.defaultValue(projName, type).toLowerCase();
            }
        },
        COMP_NAME("Component name (e.g. 'sun-xslt-engine' or 'sun-http-binding'") {
            public String defaultValue(String projName, CompType type) {
                return "sun-"+ PROJ_SHORT_LC.defaultValue(projName, type) +
                        "-"+ type.getType();
            }
        },
        COMP_TYPE_ATTR("Component type attribute value, must be 'service-engine' or 'binding-component'") {//@=").append(typeDesc.replace(" ", "-").toLowerCase()).append("\n\n")    // service-engine
            public String defaultValue(String projName, CompType type) {
                return type.getAttr();
            }
        },
        COMP_TYPE_ENUM("Component type enum name, must be 'ServiceEngine' or 'BindingComponent'") {//@=").append(typeDesc.replace(" ", "")).append("\n\n")   // ServiceEngine
            public String defaultValue(String projName, CompType type) {
                return type.getEnum();
            }
        },
        COMP_TYPE_DESC("Component type display name, used in javadoc") {//@=").append(typeDesc).append("\n\n")    // Service Engine
            public String defaultValue(String projName, CompType type) {
                return type.getDesc();
            }
        },
        SERVICE_DEF_NAME("The unqualified classname of Endpoint's service definition") {//@=Object").append("\n\n")     // Object
            public String defaultValue(String projName, CompType type) {
                return "Object";
            }
        },
        LOG_PREFIX("The logging id prefix, should be 3-6 uppercase characters") {//@=").append(prefix.toUpperCase()).append("\n\n")    // XQUERYSE
            public String defaultValue(String projName, CompType type) {
                return CLASS_PREFIX.defaultValue(projName, type).toUpperCase();
            }
        },
        AUTHOR("The value for the @author javadoc tags") {
            public String defaultValue(String projName, CompType type) {
                return "CDK";
            }
        },
        PROFILE("The name of the creation profile to use") {
            public String defaultValue(String projName, CompType type) {
                return "ojc-core";
            }
        };

        private String mDesc;
        
        private Tkn(String desc) {
            mDesc = desc;
        }
        
        public abstract String defaultValue(String projName, CompType type);

        public String camelCase(String str) {
            return (Util.isEmpty(str)) 
                    ? str : str.substring(0, 1).toUpperCase() + str.substring(1);
        }
        
        public String getDescription() {
            return mDesc;
        }
        
        public String toKey() {
            return "%"+ toString() +"%";
        }
    }
    
    public static class TokenGen {
        private String mProjName;
        private CompType mCompType;
        private File mProjRoot, mTokenFile;
        private File mScrubConfig;
        private String mProfileName;
        private Map<Tkn, String> mTokens;
        
        public TokenGen(File tokenFile) {
            this(null, null, tokenFile, null, null);
        }
        
        public TokenGen(CompType compType, File projRoot, File tokenFile,
                        File scrubConfig, String profileName) {
            mCompType = compType;
            mProjRoot = projRoot;
            mProjName = (projRoot == null) ? "" : projRoot.getName();
            mTokenFile = tokenFile;
            mScrubConfig = scrubConfig;
            mProfileName = profileName;
            mTokens = new HashMap<Tkn, String>();
        }

//        public CompType getCompType() {
//            return mCompType;
//        }
        
        public Map<Tkn, String> getTokens() {
            return mTokens;
        }
        
        public void initDefaultTokens() {
            for (Tkn tkn : Tkn.values()) {
//                System.out.println(tkn);
                if (Util.isEmpty(mTokens.get(tkn))) {
                    mTokens.put(tkn, tkn.defaultValue(mProjName, mCompType));
                }
            }
        }
        
        public void scrubProjectFiles() throws Exception {
            // load tokens
            System.out.println("Scrubbing "+ mProjRoot.getAbsolutePath() 
                    +" using profile \""+ mProfileName +"\" in "
                    + mScrubConfig.getAbsolutePath());
            Properties props = new Properties();
            props.load(new FileInputStream(mTokenFile));
            // load the scrub profile
            XPathElement config = XPathElement.loadXmlFile(mScrubConfig);
//            XmlObject<ScrubExpr> config = 
//                new XmlObject<ScrubExpr>(mScrubConfig, ScrubExpr.values());
            // find profile and iterate through files to scrub
            String xpath = ScrubExpr.profiles.query("[@name = '", mProfileName, "']");
            Element profElem = (Element) config.getNode(xpath);
//                    config.evaluate(xpath, config.getElement(), XPathConstants.NODE);
            if (profElem == null) { 
                throw new ProjectException(I18n.loc(    // XXX
                        "No definition for profile: {0}", mProfileName));
            }
            
            Profile profile = new Profile(profElem, mScrubConfig, mProfileName, mProjRoot, props);
            // Scrubbers are recursive, from file on down...
            for (Iterator<Scrubber> iter = profile.scrubbers(); iter.hasNext();) {
                Scrubber scrubber = iter.next();
                scrubber.scrub();
                scrubber.save();
            }
        }
        
        public void setCompType(CompType type) {
            mCompType = type;
            getTokens().put(Tkn.COMP_TYPE_DESC, type.getDesc());
        }
        
        public void setProjectName(String projName) {
            mProjName = projName;
            getTokens().put(Tkn.PROJ_NAME, projName);
            if (mProjRoot != null && !mProjRoot.getName().equals(projName)) {
                setProjectRoot(new File(mProjRoot, projName));
            }
        }
        
        public void setProjectRoot(File projRoot) {
            mProjRoot = projRoot;
            getTokens().put(Tkn.PROJ_ROOT, 
                    projRoot.getAbsolutePath().replace("\\", "\\\\"));
        }
        
        public void writeTokens(Map<Tkn, String> tokens) throws IOException {
            // generate token properties file
            StringBuffer buff = new StringBuffer();
            buff.append("# GENERATED BY CDK\n")
//                .append("# EDIT THESE PROPERTIES TO CUSTOMIZE COMPONENT PROJECT CREATION\n")
//                .append("# To generate new project using this file, from the command-line:\n")
//                .append("#     ant -Dtoken.file=<this-file> custom\n")
                .append("# =============================================================\n\n");
            for (Tkn tkn : tokens.keySet()) {
                buff.append("# ").append(tkn.getDescription()).append("\n")
                    .append("%").append(tkn.toString()).append("%=")
                    .append(tokens.get(tkn)).append("\n\n");
            }
            
            // System.out.println(buff.toString());
            // System.out.println(tknFile.getAbsolutePath());
            Util.writeFile(mTokenFile, buff.toString());
        }
    }
    
    /**
     * @param args
     */
    public static void main(String[] args) {
        if (args != null && args.length >= 3) {
            try {
                // acquire args
                CompType compType = CompType.valueOf(args[0]);
                File projRoot = new File(args[1]), tknFile = new File(args[2]);
                // only when scrubbing, these are passed
                File scrubConfig = null;
                String profile = null;
                
                // create tokens?
                boolean write = true;
                if (args.length > 3) {
                    scrubConfig = new File(args[3]);
                    profile = (args.length > 4) 
                            ? args[4] : Scrubber.DEFAULT_PROFILE;
                    if (Util.isEmpty(profile) || profile.startsWith("${")) {
                        profile = Scrubber.DEFAULT_PROFILE;
                    }
                    write = false;
                }

                TokenGen tknGen = new TokenGen(compType, projRoot, tknFile,
                                               scrubConfig, profile);
                // create tokens
                if (write) {
                    System.out.println("*********************** WRITING DEFAULT TOKENS **********************************");
                    tknGen.initDefaultTokens();
                    tknGen.writeTokens(tknGen.getTokens());
                    
                }
                else {
                    tknGen.scrubProjectFiles();   // scrub-config
                }
            }
            catch (Exception e) {
                throw new ProjectException(I18n.loc(
                        "Failed to generate template tokens: {0}", e.getMessage()),
                        e);
            }
        }
    }
}
