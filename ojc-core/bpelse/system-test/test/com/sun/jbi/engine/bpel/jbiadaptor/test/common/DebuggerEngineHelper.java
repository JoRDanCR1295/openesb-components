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
 * @(#)DebuggerEngineHelper.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.jbiadaptor.test.common;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;
import java.util.logging.Logger;

import javax.wsdl.Binding;
import javax.wsdl.Definition;
import javax.wsdl.Operation;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.xml.namespace.QName;
import javax.xml.parsers.ParserConfigurationException;

import org.xml.sax.EntityResolver;
import org.xml.sax.SAXException;

import com.sun.bpel.model.BPELDocument;
import com.sun.bpel.model.BPELDocumentParseFactory;
import com.sun.bpel.model.BPELParseContext;
import com.sun.bpel.model.DefaultWSDLResolverFactory;
import com.sun.bpel.model.PartnerLink;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.bpel.model.meta.RStartElement;
import com.sun.bpel.model.parser.impl.ParseContextImpl;
import com.sun.bpel.model.util.ParsingCaches;
import com.sun.bpel.model.visitor.IWSDLResolver;
import com.sun.jbi.engine.bpel.DeploymentBindings;
import com.sun.jbi.engine.bpel.PortMapEntry;
import com.sun.jbi.engine.bpel.PortMapReader;
import com.sun.jbi.engine.bpel.DeploymentBindings.InComingKey;
import com.sun.jbi.engine.bpel.EngineHelper.ActivationEntry;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.engine.InComingEventModel;
import com.sun.jbi.engine.bpel.core.bpel.engine.InComingEventModelFactory;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage;
import com.sun.jbi.engine.bpel.impl.BPELSEException;
import com.sun.jbi.engine.bpel.jbiadaptor.test.bpeldebugger.DebuggerEngine;
import com.sun.jbi.engine.bpel.jbiadaptor.test.bpeldebugger.DebuggerEngine.RefKey;
import com.sun.jbi.engine.bpel.jbiadaptor.test.bpeldebugger.DebuggerEngine.TestBPELProcessManagerImpl;
import com.sun.org.apache.xml.internal.serialize.OutputFormat;
import com.sun.org.apache.xml.internal.serialize.XMLSerializer;
import com.sun.wsdl4j.ext.DeferredActionRegistry;
import com.sun.wsdl4j.ext.WSDL4JExt;
import com.sun.wsdl4j.ext.bpel.PartnerLinkRole;
import com.sun.wsdl4j.ext.bpel.PartnerLinkType;

public class DebuggerEngineHelper {

	/** PARTNER_MYROLE constant */
	public static final String PARTNER_MYROLE = "myRole";

	/** PARTNER_PARTNERROLE constant */
	public static final String PARTNER_PARTNERROLE = "partnerRole";

	private static Logger mLogger = Logger
			.getLogger("com.sun.jbi.engine.bpel.jbiadaptor.test.bpeldebugger.DebuggerEngineHelper");


	public static PartnerLink findPartner(RBPELProcess bp, QName partnerLink) {
		String namespace = partnerLink.getNamespaceURI();
		String localpart = partnerLink.getLocalPart();
		PartnerLink ret = null;

		if (namespace.equals(bp.getTargetNamespace())) {
			Collection partners = bp.getPartnerLinks().getPartnerLinks();

			for (Iterator itr = partners.iterator(); itr.hasNext();) {
				PartnerLink partner = (PartnerLink) itr.next();

				if (partner.getName().equals(localpart)) {
					ret = partner;

					break;
				}
			}
		}

		return ret;
	}

	public static PortType findPortType(RBPELProcess bp,
			QName partnerlinktype, String roleName) {
		Iterator defIter = bp.getAllImportedWSDLDefinitions().iterator();
		PartnerLinkType slt = null;
		while (defIter.hasNext()) {
			Definition def = (Definition) defIter
					.next();
			slt = WSDL4JExt.getPartnerLinkType(def, partnerlinktype);

			if (slt != null) {
				PartnerLinkRole role = slt.getRole(roleName);
				if (role != null) {
					return role.getPort();
				}
			}
		}

		return null;
	}

	/** Creates a new instance of EngineDriver */
	public static void serialize(WSMessage msg, FileOutputStream fos) {
		try {
			OutputFormat outFormat = new OutputFormat("XML", "UTF-8", true);
			outFormat.setPreserveSpace(false);

			//outFormat.setLineSeparator("");
			XMLSerializer serializer = new XMLSerializer();
			serializer.setOutputFormat(outFormat);
			serializer.setOutputByteStream(fos);
			serializer.serialize(msg.getElement());
		} catch (Exception e) {
			e.printStackTrace();
		}
	}


	public static List deploy(Engine engine,
			MasterTestProperties masterTestProperties,
			DeploymentBindings depBindings,
			Collection startedBPELIds, File outputFile, Map modelMap)
			throws BPELSEException {
		List ret = new ArrayList();

		Set<Entry<String, TestProperties>> testsEntrySet = masterTestProperties.deploys
				.entrySet();
		List<File> deployDirs = new ArrayList<File>();
		File baseFile = masterTestProperties.baseDir;
		for (Iterator<Entry<String, TestProperties>> it = testsEntrySet
				.iterator(); it.hasNext();) {
			Entry<String, TestProperties> testEntry = it.next();
			File bpelDir = new File(baseFile, testEntry.getValue().name);
			deployDirs.add(bpelDir);
		}

		List bpelfiles = new ArrayList();
		List portMapList = new ArrayList();

		for (int i = 0; i < deployDirs.size(); i++) {
			File bpelDir = deployDirs.get(i);
			File[] childFiles = bpelDir.listFiles();

			for (int j = 0; j < childFiles.length; j++) {
				File file = childFiles[j];
				if (file.isFile()) {
					if (file.getName().endsWith(".bpel")) {
						bpelfiles.add(file.getAbsolutePath());
					} else if (file.getName().endsWith("portmap.xml")) {
						String portmapfile = file.getAbsolutePath();
						portMapList.add(portmapfile);
					}
				}
			}
		}
		List bps = loadDefinitions(masterTestProperties, deployDirs, engine,
				startedBPELIds, outputFile, modelMap, depBindings);
        
		/*if ((epsHelper != null)
				&& epsHelper instanceof EndpointStatisticsHelper) {*/
			Iterator portmaps = null;
			ArrayList portmapsIterList = new ArrayList();

			for (Iterator portmapIter = portMapList.iterator(); portmapIter
					.hasNext();) {
				String portmapfile = (String) portmapIter.next();
				try {
					portmaps = PortMapReader.parse(new File(portmapfile));
				} catch (SAXException ex) {
					throw new BPELSEException("unable to parse portmap file: "
							+ portmapfile, ex);
				} catch (IOException ex) {
					throw new BPELSEException("unable to read portmap file: "
							+ portmapfile, ex);
				} catch (ParserConfigurationException ex) {
					throw new BPELSEException("unable to parse portmap file: "
							+ portmapfile, ex);
				}
				portmapsIterList.add(portmaps);
			}

			for (Iterator portmapListIter = portmapsIterList.iterator(); portmapListIter
					.hasNext();) {
				portmaps = (Iterator) portmapListIter.next();
				while ((portmaps != null) && portmaps.hasNext()) {
					PortMapEntry entry = (PortMapEntry) portmaps.next();
					if (entry.getServiceName() == null
							|| entry.getServiceName().equals(
									new javax.xml.namespace.QName(""))) {
						continue;
					}

					if (entry.getRole().equalsIgnoreCase(PARTNER_MYROLE)) {
						// inbound
						QName serviceName = entry.getServiceName();
						QName partnerLink = entry.getPartnerLink();
						QName partnerLinkType = entry.getPartnerLinkType();
						QName endPoint = entry.getEndPoint();
						String roleName = entry.getRoleName();
						Iterator bpsit = bps.iterator();
						PartnerLink myPartner = null;
						RBPELProcess bp = null;
						boolean foundOper = false;

						/*******************************************************
						 * ************************************************************************************ //
						 * TODO performance can be improved (using the
						 * foundOper) flag, if // portMap provides aditional
						 * hints as to how many BPELS had the same //
						 * partnerLink defined.
						 ******************************************************/

						// while (bpsit.hasNext() && !foundOper) {
						while (bpsit.hasNext()) {
							bp = (RBPELProcess) bpsit.next();

							myPartner = findPartner(bp, partnerLink);

							if (myPartner == null) {
								continue;
							}

							foundOper = false;

							Iterator actIt = bp.getStartElements().iterator();

							while (actIt.hasNext()) {
								RStartElement se = (RStartElement) actIt.next();
								PortType pt = findPortType(
										bp, partnerLinkType, roleName);

								/** TODO EPS */

								// myPartner.setMyService(serviceName);
								// myPartner.setMyPortType(pt);
								/** TODO EPS */
								QName saPT = se.getRPortType();

								if (pt == null || !pt.getQName().equals(saPT)) {
									continue;
								}
								
								String opName = se.getWSDLOperation().getName();
								List<Operation> operations = pt.getOperations();
								ArrayList<Operation> selected = new ArrayList<Operation>();
								for (Operation op : operations) {
								    if (opName != null && opName.equals(op.getName())) {
								        selected.add(op);
								    }
								}

								Iterator<Operation> opIter = selected.iterator();

								while (opIter.hasNext()) {
									Operation op = opIter.next();

									// uses the first Operation that matches
									// start activity
									String input = op.getInput().getName();
									QName ipMsgType = op.getInput().getMessage().getQName();
									String output = null;
									QName opMsgType = null;
									String pattern = Engine.IN_ONLY;

									if (op.getOutput() != null) {
										output = op.getOutput().getName();
										opMsgType = op.getOutput().getMessage().getQName();
										pattern = Engine.IN_OUT;
									}

									// TODO do this similar for faults.
									InComingKey key = depBindings
											.createInComingBindingsKey(
													serviceName, endPoint
															.getLocalPart(), op
															.getName());
									InComingEventModel model = InComingEventModelFactory
											.createModel(bp, se, pattern);
									depBindings.addInComingEventModel("TestSU", key,
											model);

                                    /*epsHelper.registerInboundEndpoint(entry.getServiceName(), endPoint
                                            .getLocalPart(), "TestSU");*/

									engine.addStartActivityModel(bp, se,
											pattern);

									break;
								}

								ret.add(new ActivationEntry(serviceName, endPoint
										.getLocalPart()));
								mLogger.info("activated inbound service: "
										+ serviceName);
							}
						}
					} else if (entry.getRole().equalsIgnoreCase(
							PARTNER_PARTNERROLE)) {
						// outbound
						QName serviceName = entry.getServiceName();
						QName partnerLink = entry.getPartnerLink();
						QName partnerLinkType = entry.getPartnerLinkType();
						QName endPoint = entry.getEndPoint();
						String roleName = entry.getRoleName();
						Iterator bpsit = bps.iterator();
						PartnerLink partnerPartner = null;
						boolean foundBPEL = false;

						/*******************************************************
						 * ************************************************************************************ //
						 * TODO performance can be improved (using the
						 * foundOper) flag, if // portMap provides aditional
						 * hints as to how many BPELS had the same //
						 * partnerLink defined.
						 ******************************************************/

						// while (bpsit.hasNext() && !foundBPEL) {
						while (bpsit.hasNext()) {
							foundBPEL = false;

							RBPELProcess bp = (RBPELProcess) bpsit.next();
							partnerPartner = findPartner(bp, partnerLink);

							if (partnerPartner == null) {
								// foundBPEL is already false.
								continue;
							}

							PortType pt = findPortType(bp, partnerLinkType,
									roleName);

							if (pt == null) {
								// foundBPEL is already false.
								continue;
							}

							foundBPEL = true;

							// partnerPartner.setPartnerPortType(pt);
							// partnerPartner.setPartnerService(serviceName);

							/*epsHelper.registerOutboundEndpoint(serviceName,
                                                                endPoint.getLocalPart(), "TestSU");*/
						}
					}
				}
			}
		//}

		return ret;
	}


	public static  List loadDefinitions(MasterTestProperties masterTestProperties, List <File> deploydirs, Engine engine,
			Collection startedBPELIds, File outputFile, Map modelMap, DeploymentBindings depBindings) throws BPELSEException {

		List bpelfiles = new ArrayList();
		ArrayList bpelList = new ArrayList();

		for (int i = 0; i < deploydirs.size (); i++) {
			File bpelDir = deploydirs.get(i);
			File [] childFiles = bpelDir.listFiles();

			for (int j =0 ; j<childFiles.length; j++) {
				File file = childFiles[j];
				if (file.isFile()) {
					if (file.getName().endsWith(".bpel")) {
						bpelfiles.add(file);
					}
				}
			}
		}

		HashSet ret = new HashSet();
		URL url = null;
		InputStream is = null;
		InputStreamReader reader = null;
//		File catalogFile = new File(deploydir, "xml-catalog.xml");
		EntityResolver resolver = null;

//		if (catalogFile.exists()) {
//			CatalogManager catalogManager = new CatalogManager();
//			catalogManager.setCatalogFiles(catalogFile.getAbsolutePath());
//			catalogManager.setRelativeCatalogs(true);
//			catalogManager.setUseStaticCatalog(false);
//			resolver = new CatalogResolver(catalogManager);
//		} else {
//			String msg = "XML catalog not found, "
//					+ catalogFile.getAbsolutePath() + ", does not exist.";
//			mLogger.warning(msg);
//		}

		try {
			for (int i = 0; i < bpelfiles.size(); i++) {
				File bpelFile = (File) bpelfiles.get(i);

				String bpelDir = bpelFile.getParentFile().getName();
				String bpelfilepath = bpelFile.getAbsolutePath();

				try {
					url = new URL("file", null, bpelfilepath);
					is = url.openStream();
					reader = new InputStreamReader(is);

                    BPELParseContext parseContext = new ParseContextImpl();
                    parseContext.setCatalog(bpelFile.getParentFile(), bpelFile);
					String bpelFileURI = bpelFile.toURI().toString();
					IWSDLResolver wsdlResolver = DefaultWSDLResolverFactory
							.getInstance().newWSDLResolver(bpelFileURI,
									parseContext);
					parseContext.setWSDLResolver(wsdlResolver);

		            ParsingCaches caches = new ParsingCaches();
		            parseContext.setCaches(caches);
			        DeferredActionRegistry registry = new DeferredActionRegistry();
			        parseContext.setDeferredActionRegistry(registry);
					BPELDocument bpelDoc = BPELDocumentParseFactory
							.getInstance().load(reader, parseContext);
                    WSDL4JExt.applySingleSchemaTypeLoader(registry, parseContext.getBaseURIResolver());
					bpelDoc.setBaseURI(bpelfilepath);

					RBPELProcess bProc = (RBPELProcess) bpelDoc
							.getDocumentProcess();

					TestProperties testProperty = (TestProperties) masterTestProperties.deploys.get(bpelDir);
					TestBPELProcessManagerImpl processManager = new TestBPELProcessManagerImpl(
							bProc, engine, testProperty, bpelFile.getParentFile(), depBindings);
					Iterator actit = bProc.getStartElements().iterator();
					while (actit.hasNext()) {
						RActivity act = (RActivity) actit.next();
						if (!(act instanceof RStartElement)) {
							continue;
						}

						RStartElement sa = (RStartElement) act;
						String pattern = Engine.IN_ONLY;
						processManager.addStartActivityModel(sa, pattern);
					}

					actit = bProc.getStartElements().iterator();
					while (actit.hasNext()) {
						RActivity act = (RActivity) actit.next();
						if (!(act instanceof RStartElement)) {
							continue;
						}

						RStartElement sa = (RStartElement) act;
						PortType pt = findPortType(bProc, sa.getRPortType());
						QName service = findService(bProc, pt);
						String opname = sa.getWSDLOperation().getName();
						RefKey key = new DebuggerEngine.RefKey(opname, service);
						Collection ops = pt.getOperations();
						for (Iterator opit = ops.iterator(); opit.hasNext();) {

							Operation op = (Operation) opit.next();
							if (op.getName().equals(opname)) {
								QName type = op.getInput().getMessage().getQName();
								/*type.setNamespaceURI(op.getInput()
										.getNamespace(type.getPrefix())); */
								modelMap.put(key, new DebuggerEngine.ServiceRef(processManager, type));
								break;
							}
						}
					}

					bpelList.add(bProc);
					engine.addModel(bProc, processManager);
					startedBPELIds.add(bProc.getBPELId());
					ret.add(bProc);
					reader.close();
				} catch (MalformedURLException ex) {
					throw new BPELSEException("bpel file url[" + bpelfilepath
							+ "] is malformed", ex);
				} catch (IOException ex) {
					throw new BPELSEException(
							"unable to open bpel file stream for "
									+ bpelfilepath, ex);
				} catch (Exception ex) {
                    throw new BPELSEException(
                            "Exceptions occurs for  "
                                    + bpelfilepath, ex);

                }
			}
		} finally {
			if (is != null) {
				try {
					is.close();
				} catch (IOException ex) {
					throw new BPELSEException(ex);
				}
			}

			if (reader != null) {
				try {
					reader.close();
				} catch (IOException ex) {
					throw new BPELSEException(ex);
				}
			}
		}

		return bpelList;
	}

	public static List findBPELFiles(File dir) {
		List ret = new ArrayList();
		File[] files = dir.listFiles();

		for (int i = 0; i < files.length; i++) {
			File file = files[i];

			if (file.isFile() && file.getName().endsWith(".bpel")) {
				ret.add(file.getAbsolutePath());
			} else if (file.isDirectory()) {
				ret.addAll(findBPELFiles(file));
			}
		}

		return ret;
	}

   public static QName findService(Definition def, PortType pt) {
        Iterator defit = def.getBindings().values().iterator();
        Binding binding = null;
        while (defit.hasNext()) {
            Binding b = (Binding) defit.next();
            if (b.getPortType().getQName().equals(pt.getQName())) {
                binding = b;
                break;
            }
        }       
        if (binding != null) {
            Iterator servit = def.getServices().values().iterator();
            while (servit.hasNext()) {
                Service s = (Service) servit.next();
                Iterator portit = s.getPorts().values().iterator();
                boolean found = false;
                while (portit.hasNext()) {
                    Port p = (Port) portit.next();
                    if (p.getBinding().getQName().getLocalPart().equals(
                            binding.getQName().getLocalPart())) {
                        found = true;
                        break;
                    }
                }

                if (found) {
                    return s.getQName();
                }
            }
        }
        return null;        
       
   }

	public static QName findService(RBPELProcess bp, PortType pt) {
		Collection wsdls = bp.getImportedWSDLDefinitions(pt.getQName()
				.getNamespaceURI());
		List wsdllist = new ArrayList(wsdls);
		Binding binding = null;
		Definition theDef = null;
		if (wsdllist != null) {
			done: for (int i = 0; i < wsdllist.size(); i++) {
				Definition def = (Definition) wsdllist.get(i);
				Iterator defit = def.getBindings().values().iterator();
				while (defit.hasNext()) {
					Binding b = (Binding) defit.next();
					if (b.getPortType().getQName().equals(pt.getQName())) {
						binding = b;
						theDef = def;
						break done;
					}
				}
			}
		}

		if (binding != null) {
			Iterator servit = theDef.getServices().values().iterator();
			while (servit.hasNext()) {
				Service s = (Service) servit.next();
				Iterator portit = s.getPorts().values().iterator();
				boolean found = false;
				while (portit.hasNext()) {
					Port p = (Port) portit.next();
					if (p.getBinding().getQName().getLocalPart().equals(
							binding.getQName().getLocalPart())) {
						found = true;
						break;
					}
				}

				if (found) {
					return s.getQName();
				}
			}
		} else { // no binding information, assuming one service provided in the set of WSDL files
			for (int i = 0; i < wsdllist.size(); i++) {
				Definition def = (Definition) wsdllist.get(i);
				if (!def.getServices().isEmpty()) {
					Iterator servit = def.getServices().values().iterator();
					while (servit.hasNext()) {
						return ((Service) servit.next()).getQName(); // return the firs service defined
					}
				}
			}
		}

		return null;
	}


	public static PortType findPortType(RBPELProcess bp,
			javax.xml.namespace.QName name) {
		Collection wsdls = bp
				.getImportedWSDLDefinitions(name.getNamespaceURI());
		List wsdllist = new ArrayList(wsdls);
		PortType ret = null;
		if (wsdllist != null) {
			done: for (int i = 0; i < wsdllist.size(); i++) {
				Definition def = (Definition) wsdllist.get(i);
				Iterator defit = def.getPortTypes().values().iterator();
				while (defit.hasNext()) {
					PortType p = (PortType) defit.next();
					if (p.getQName().getNamespaceURI().equals(
							name.getNamespaceURI())
							&& p.getQName().getLocalPart().equals(
									name.getLocalPart())) {
						ret = p;

						break done;
					}
				}
			}
		}

		return ret;
	}

}
