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
 * @(#)AsAdmin.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.project.util;

import java.io.File;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import javax.swing.ImageIcon;
import com.sun.jbi.component.toolkit.project.model.Project;
import com.sun.jbi.component.toolkit.project.util.Build.Ant;
import com.sun.jbi.component.toolkit.project.view.App;
import com.sun.jbi.component.toolkit.project.view.App.Status;

/**
 * 
 * @author Kevan Simpson
 */
public enum AsAdmin implements Exec {
    start_jbi_component,
    stop_jbi_component,
    shut_down_jbi_component,
    build_installer() {
        /** @see com.sun.jbi.component.toolkit.project.util.AsAdmin#newExec() */
        @Override
        Exec newExec() {
            return Ant.build_installer;
        }
    },
    install_jbi_component() {
        /** @see com.sun.jbi.component.toolkit.project.util.AsAdmin#getArgs(com.sun.jbi.component.toolkit.project.model.Project) */
        @Override
        public List<String> getArgs(Project proj) {
            List<String> args = super.getArgs(proj);
            File jar = proj.getInstallerJar();
            if (jar == null) {
                args.clear();
            }
            else {
                // replace component-name arg with installer jar absolute path
                args.set(2, jar.getAbsolutePath());
            }
            
            return args;
        }

        /** @see com.sun.jbi.component.toolkit.project.util.AsAdmin#execute(com.sun.jbi.component.toolkit.project.model.Project) */
        @Override
        public String[] execute(Project proj, String... args) {
            if (getArgs(proj).isEmpty()) {
                return new String[] {
                        "The installer jar does not exist...",
                        "Please build the component prior to installation.",
                        "Click the 'Build' button for instructions."
                };
            }
            return super.execute(proj);
        }
    },
    uninstall_jbi_component() {
        /** @see com.sun.jbi.component.toolkit.project.util.AsAdmin#execute(com.sun.jbi.component.toolkit.project.model.Project, java.lang.String[]) */
        @Override
        public String[] execute(Project proj, String... args) {
            // ensure component is shutDown before uninstalling
            String[] sdjc = shut_down_jbi_component.execute(proj);
            // merge output, adding FYI message re: shutDown
            List<String> output = new ArrayList<String>();
            if (sdjc != null) {
                for (String line : sdjc) {
                    output.add(line);
                }
            }
            output.add("If component was already shutDown, please ignore the previous message.");
            
            String[] ujc = super.execute(proj);
            if (ujc != null) {
                for (String line : ujc) {
                    output.add(line);
                }
            }
            
            return output.toArray(new String[output.size()]);
        }
        
    },
    list_jbi_service_assemblies() {
        /** @see com.sun.jbi.component.toolkit.project.util.AsAdmin#getArgs(com.sun.jbi.component.toolkit.project.model.Project) */
        @Override
        public List<String> getArgs(Project proj) {
            List<String> args = super.getArgs(proj);
            args.add(2, "--componentname");
            return args;
        }
    },
    start_domain() {
        /** @see com.sun.jbi.component.toolkit.project.util.AsAdmin#getArgs(com.sun.jbi.component.toolkit.project.model.Project) */
        @Override
        public List<String> getArgs(Project proj) {
            List<String> args = super.getArgs(proj);
//            args.set(args.size() - 1, "--debug=true");
            args.remove(args.size() - 1);
            return args;
        }
    },
    debug_domain() {
        /** @see com.sun.jbi.component.toolkit.project.util.AsAdmin#getArgs(com.sun.jbi.component.toolkit.project.model.Project) */
        @Override
        public List<String> getArgs(Project proj) {
            List<String> args = new ArrayList<String>();
            
            args.add(proj.getAsAdmin().getAbsolutePath());
            args.add(start_domain.toString());
            args.add("--debug=true");

            return args;
        }
    },
    stop_domain() {
        /** @see com.sun.jbi.component.toolkit.project.util.AsAdmin#getArgs(com.sun.jbi.component.toolkit.project.model.Project) */
        @Override
        public List<String> getArgs(Project proj) {
            List<String> args = super.getArgs(proj);
            args.remove(args.size() - 1);
            return args;
        }
    },
    show_jbi_service_engine(),
    show_jbi_binding_component();
    
    /** @see com.sun.jbi.component.toolkit.project.util.Exec#execute(com.sun.jbi.component.toolkit.project.model.Project, java.lang.String[]) */
    public String[] execute(Project proj, String... args) {
        if (proj != null && proj.getAsAdmin() != null) {
            Exec exec = newExec();
            return exec.execute(proj, toArray(getArgs(proj)));
        }            
        
        return new String[0];
    }

    public List<String> getArgs(Project proj) {
        List<String> args = new ArrayList<String>();
        args.add(proj.getAsAdmin().getAbsolutePath());
        args.add(this.toString());
        args.add(proj.getComponent().getName());
        
        return args;
    }
    
    Exec newExec() {
        return new Cmd(this.toString());
    }
    
    String[] toArray(List<String> list) {
        return list.toArray(new String[list.size()]);
    }

    /** @see java.lang.Enum#toString() */
    @Override
    public String toString() {
        return super.toString().replace('_', '-');
    }

    public static class AsAdminAction extends CmdAction {
        private AsAdmin mAsAdmin;
        
        public AsAdminAction(String desc, App app, AsAdmin cmd, int mnemonic) {
            super(desc, app, new Cmd(cmd.toString()), mnemonic);
            mAsAdmin = cmd;
            initIcon();
        }

        
        /** @see com.sun.jbi.component.toolkit.project.util.CmdAction#execute(com.sun.jbi.component.toolkit.project.model.Project, java.lang.String[]) */
        @Override
        public String[] execute(Project proj, String... args) {
            // ignore those passed in...which will be never
            return super.execute(
                    proj, mAsAdmin.toArray(mAsAdmin.getArgs(getApp().getProject())));
        }


        /** @see com.sun.jbi.component.toolkit.project.util.CmdAction#getExec() */
        @Override
        protected Exec getExec() {
            return mAsAdmin.newExec();
        }

        /** @see com.sun.jbi.component.toolkit.project.util.CmdAction#showOutput(java.lang.String[]) */
        @Override
        protected void showOutput(String... output) {
            switch (mAsAdmin) {
//                case start_domain:
//                    break;
                case stop_domain: {
                    getApp().showMessages(Status.good, output);
                    getApp().showMessages(Status.info, "Command "+ getCmdName() +" executed successfully!");
                    break;
                }
//                case debug_domain:
//                    break;
//                case build_installer:
//                    break;
//                case install_jbi_component:
//                    break;
//                case uninstall_jbi_component:
//                    break;
//                case start_jbi_component:
//                    break;
//                case stop_jbi_component:
//                    break;
//                case shut_down_jbi_component:
//                    break;
//                case show_jbi_service_engine:
//                case show_jbi_binding_component: {
//                    break;
//                }
//                case list_jbi_service_assemblies:
//                    break;
                default: {
                    super.showOutput(output);
                }
            }
        }

        private void initIcon() {
            String path = null;
            switch (mAsAdmin) {
                case start_domain:
                    path = "icons/as-start.png";
                    break;
                case stop_domain:
                    path = "icons/as-stop.png";
                    break;
                case debug_domain:
                    path = "icons/as-debug.png";
                    break;
                case build_installer:
                    path = "icons/comp-build.png";
                    break;
                case install_jbi_component:
                    path = "icons/comp-install.png";
                    break;
                case uninstall_jbi_component:
                    path = "icons/comp-uninstall.png";
                    break;
                case start_jbi_component:
                    path = "icons/comp-start.png";
                    break;
                case stop_jbi_component:
                    path = "icons/comp-stop.png";
                    break;
                case shut_down_jbi_component:
                    path = "icons/comp-shutdown.png";
                    break;
                case show_jbi_service_engine:
                    path = "icons/comp-status.png";
                    break;
                case show_jbi_binding_component:
                    path = "icons/comp-status.png";
                    break;
                case list_jbi_service_assemblies:
                    path = "icons/comp-manage.png";
                    break;
            }
            if (path != null) {
                URL url = getClass().getResource(path);
                putValue(SMALL_ICON, new ImageIcon(url));
            }
        }
    }
}
