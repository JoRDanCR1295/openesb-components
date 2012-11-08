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
 * @(#)DefaultRMIService.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.netbeans.modules.bpel.debuggerbdi.rmi.wp.impl;


import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.ObjectStreamClass;
import java.io.Reader;
import java.io.Serializable;
import java.io.StringReader;
import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
import java.lang.reflect.Array;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.net.ConnectException;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;
import java.net.UnknownHostException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.ResourceBundle;
import java.util.Vector;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.RejectedExecutionException;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.netbeans.modules.bpel.debuggerbdi.rmi.api.DebugListener;
import org.netbeans.modules.bpel.debuggerbdi.rmi.api.XpathExpressionException;
import org.netbeans.modules.bpel.debuggerbdi.rmi.wp.ObjectAdapter;
import org.netbeans.modules.bpel.debuggerbdi.rmi.wp.RMIClient;
import org.netbeans.modules.bpel.debuggerbdi.rmi.wp.RMILocationForwardException;
import org.netbeans.modules.bpel.debuggerbdi.rmi.wp.RMIServer;
import org.netbeans.modules.bpel.debuggerbdi.rmi.wp.RMIService;

/**
 * This class implements the key interfaces of com.sun.jbi.rmi.api
 *
 * @author Sun Microsystems
 */
public class DefaultRMIService implements RMIService {
    
    private static final Logger LOGGER  = Logger.getLogger(DefaultRMIService.class.getName());
    private static boolean isDebuggable = LOGGER.isLoggable(Level.FINE);

    static final Object[] PRIMITIVES = {
            boolean.class, "Z", byte.class, "B", char.class, "C", double.class, "D", float.class,
            "F", int.class, "I", long.class, "J", short.class, "S", void.class, "V"
        };

    static final Object[] BOXED = {
            Boolean.class, "Z", Byte.class, "B", Character.class, "C", Double.class, "D",
            Float.class, "F", Integer.class, "I", Long.class, "J", Short.class, "S", Void.class, "V"
        };

    static final int METHOD_REQ = 1;
    static final int METHOD_RESULT = 2;
    static final int METHOD_EXCEPTION = 3;
    static HashMap clientSessionPool = new HashMap();
    static HashMap methodCache = new HashMap();
    
    ResourceBundle rb = ResourceBundle.getBundle("org.netbeans.modules.bpel.debuggerbdi.rmi.wp.impl.Bundle");
    ClassLoader classLoader;
    HashMap servers = new HashMap();
    ExecutorService threadPool = Executors.newFixedThreadPool(20);
    boolean alive;

    
    /////////////////////////////// public ///////////////////////////
    /**
     * Creates a new DefaultRMIService object.
     *
     * @param classLoader DOCUMENT ME!
     */
    public DefaultRMIService(ClassLoader classLoader) {
        alive = true;
        this.classLoader = classLoader;
    }

    public static String getParameterTypes(Method m) {
        Class[] params = m.getParameterTypes();
        StringBuffer buf = new StringBuffer();

        for (int i = 0; i < params.length; i++) {
            addParam(params[i], buf);
        }

        buf.append(0);

        return buf.toString();
    }

    public static Class[] getParameterTypes(String sig, ClassLoader classLoader) {
        LinkedList list = new LinkedList();
        StringReader reader = new StringReader(sig);

        while (true) {
            try {
                Class c = getParam(reader, classLoader);

                if (c == null) {
                    break;
                }

                list.add(c);
            } catch (Exception exc) {
                LOGGER.log(Level.WARNING, exc.getMessage());
                break;
            }
        }

        Class[] result = new Class[list.size()];
        list.toArray(result);

        return result;
    }

    public static boolean isPrimitiveArray(Object obj) {
        Class clazz = obj.getClass();

        return isPrimitiveArray(clazz);
    }

    public static boolean isPrimitiveArray(Class clazz) {
        if (clazz.isArray()) {
            Class compType = clazz.getComponentType();

            if (compType.equals(String.class)) {
                return true;
            }

            for (int i = 0; i < PRIMITIVES.length; i += 2) {
                if (compType.equals(PRIMITIVES[i])) {
                    return true;
                }
            }

            for (int i = 0; i < BOXED.length; i += 2) {
                if (compType.equals(BOXED[i])) {
                    return true;
                }
            }
        }

        return false;
    }

    public RMIClient createClient(String host, int port)
        throws IOException {
        RMIClient client = null;

        if (!host.equalsIgnoreCase("localhost")) {
            client = createClient(InetAddress.getByName(host), port);
        } else {
            client = createClient(InetAddress.getLocalHost(), port);
        }

        return client;
    }

    public RMIClient createClient(InetAddress hostAddress, int port)
        throws IOException {
        RMIServer server;

        synchronized (servers) {
            if (servers.size() == 0) {
                createServer(0);
            }

            server = (RMIServer) servers.values().iterator().next();
        }

        return server.createClient(hostAddress, port);
    }

    public RMIServer createServer(int port) throws IOException {
        if (port == 0) {
            return new RMIServerImpl();
        }

        return createServer((String) null, port);
    }

    public RMIServer createServer(String host, int port)
        throws IOException {
        InetAddress address = null;

        if ((host == null) || host.equalsIgnoreCase("localhost")) {
            address = InetAddress.getLocalHost();
        } else {
            address = InetAddress.getByName(host);
        }

        return createServer(address, port);
    }

    public RMIServer createServer(InetAddress hostAddress, int port)
        throws IOException {
        RMIServer result;
        RMIConnectionFactory fac = new RMIConnectionFactory(hostAddress, port);

        synchronized (servers) {
            result = (RMIServer) servers.get(fac);

            if (result == null) {
                result = new RMIServerImpl(hostAddress, port);
            }

            servers.put(fac, result);
        }

        return result;
    }

    public void destroy() {
        alive = false;
        try {
            threadPool.shutdownNow();
        } catch (Exception e) {
            LOGGER.log(Level.WARNING, e.getMessage());
        }
    }

    static ClientSession getClientSession(RMIConnectionFactory factory) {
        ClientSession e;

        synchronized (clientSessionPool) {
            e = (ClientSession) clientSessionPool.get(factory);

            if (e == null) {
                e = new ClientSession(factory);
                clientSessionPool.put(factory, e);
            }
        }

        return e;
    }

    static Method findMethod(Class c, String methodName, String sig)
        throws NoSuchMethodException {
        Method result;
        String key = c.getName() + "." + methodName + "#" + sig;

        synchronized (methodCache) {
            result = (Method) methodCache.get(key);

            if (result == null) {
                try {
                    Class[] parameterTypes = getParameterTypes(sig, c.getClassLoader());
                    result = c.getMethod(methodName, parameterTypes);
                    methodCache.put(key, result);
                } catch (NoSuchMethodException exc) {
                    throw new NoSuchMethodException(
                        c.getName() + "." + methodName + "(" + sig + ")"
                    );
                }
            }
        }

        return result;
    }

    
    private static Class getParam(Reader reader, ClassLoader classLoader)
        throws Exception {
        char ch;
        ch = (char) reader.read();

        switch (ch) {
        case 'Z':
            return boolean.class;

        case 'B':
            return byte.class;

        case 'C':
            return char.class;

        case 'D':
            return double.class;

        case 'F':
            return float.class;

        case 'I':
            return int.class;

        case 'J':
            return long.class;

        case 'L':

            StringBuffer buf = new StringBuffer();

            for (ch = (char) reader.read(); ch != ';'; ch = (char) reader.read()) {
                if (ch == '/') {
                    ch = '.';
                }

                buf.append(ch);
            }

            //return classLoader.loadClass(buf.toString());
            return Class.forName(buf.toString(), true, classLoader);

        case '[':

            Class componentType = getParam(reader, classLoader);

            return Array.newInstance(componentType, 0).getClass();

        case 'S':
            return short.class;

        case 'V':
            return void.class;
        }

        return null;
    }

    private static void addParam(Class param, StringBuffer buf) {
        if (param.isArray()) {
            buf.append('[');
            addParam(param.getComponentType(), buf);
        } else {
            int i;

            for (i = 0; i < PRIMITIVES.length; i += 2) {
                if (param.equals(PRIMITIVES[i])) {
                    buf.append(PRIMITIVES[i + 1].toString());

                    break;
                }
            }

            if (i == PRIMITIVES.length) {
                buf.append('L');
                buf.append(param.getName().replace('.', '/'));
                buf.append(';');
            }
        }
    }

    
    interface Indication {
        void setSocket(RMISocket socket);
        String getAdapterName();
        Object[] getArguments();
        String getMethodName();
        String getMethodSignature();
        String getObjectKey();
        void raiseException(Throwable exc) throws IOException;
        void sendResponse(Object result) throws IOException;
    }

    
    interface Invoker {
        public void start();
    }

    
    interface Request {
        void setAdapterName(String adapterName);
        void setArguments(Object[] arguments);
        void setMethod(Method method);
        void setObjectKey(String objectKey);
        Object getResult() throws Throwable;
    }


    public static class ClientProxy implements InvocationHandler, Serializable {
        transient ClassLoader classLoader;
        transient ClientSession clientSession;
        transient RMISocket socket;
        transient ClientSession forwardSession;
        transient Object localRef;
        transient ObjectAdapter oa;
        RMIConnectionFactory factory;
        String adapterName;
        String objectKey;

        public ClientProxy(
                RMIConnectionFactory factory, 
                String adapterName, 
                String objectKey, 
                Object localRef,
                ObjectAdapter oa
        ) {
            
            if (isDebuggable) {
                LOGGER.log(Level.FINE, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX");
                LOGGER.log(Level.FINE, "Making proxy:" + localRef);
                LOGGER.log(Level.FINE, "objectKey:" + objectKey);
                LOGGER.log(Level.FINE, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX");
            }

            this.factory = factory;
            this.adapterName = adapterName;
            this.objectKey = objectKey;
            clientSession = getClientSession(factory);
            this.oa = oa;
        }

        public Object invoke(Object proxy, Method m, Object[] args)
            throws Throwable {
            Object returnObj = null;

            if (localRef != null) {
                m.setAccessible(true);

                return m.invoke(localRef, args);
            }

            boolean retry = false;
            RMISocket sock;

            if (socket == null) {
                ClientSession session;

                if (forwardSession != null) {
                    session = forwardSession;
                } else {
                    session = clientSession;
                }

                try {
                    sock = session.getSocket(oa, classLoader);
                } catch (ConnectException e) {
                    //LOGGER.log(Level.WARNING, e.getMessage());
                    throw e;
                } catch (Exception e) {
                    LOGGER.log(Level.WARNING, e.getMessage());
                    throw e;
                }
            } else {
                sock = socket;
            }

            while (!sock.isClosed()) {
                Request req = sock.createRequest();

                try {
                    req.setAdapterName(adapterName);
                    req.setObjectKey(objectKey);
                    req.setMethod(m);

                    if (args != null) {
                        Object[] newArgs = new Object[args.length];

                        for (int i = 0; i < args.length; i++) {
                            Object arg = args[i];

                            if (
                                (arg != null) && !(arg instanceof Proxy) &&
                                    !(arg instanceof String) && !(arg instanceof Number) &&
                                    !(arg instanceof Boolean) && !(isPrimitiveArray(arg))
                            ) {
                                arg = oa.exportObject(arg);
                            }

                            newArgs[i] = arg;
                        }

                        args = newArgs;
                    }

                    req.setArguments(args);
                    returnObj = req.getResult();
                    break;
                    
                } catch (SocketException exc) {
                    LOGGER.log(Level.WARNING, exc.getMessage());
                    if (retry) {
                        throw exc.fillInStackTrace();
                    }

                    sock = clientSession.getSocket(oa, classLoader);
                    forwardSession = null;
                    retry = true;
                } catch (RMILocationForwardException exc) {
                    LOGGER.log(Level.WARNING, exc.getMessage());
                    RMIConnectionFactory fac = new RMIConnectionFactory(
                            exc.getForwardHost(), exc.getForwardPort()
                        );

                    forwardSession = getClientSession(fac);
                    sock = forwardSession.getSocket(oa, classLoader);
                } catch (Throwable t) {
                    if (!(t instanceof XpathExpressionException)) {
                        LOGGER.log(Level.WARNING, t.getMessage());
                    }
                    
                    throw t;
                }
            }

            return returnObj;
        }

        void setClassLoader(ClassLoader classLoader) {
            this.classLoader = classLoader;
        }

        void setObjectAdapter(ObjectAdapter oa) {
            this.oa = oa;
        }

        private void readObject(ObjectInputStream strm)
            throws IOException, ClassNotFoundException {
            strm.defaultReadObject();
            clientSession = getClientSession(factory);
            localRef = null;
        }
    }


    public static class RMIConnectionFactory implements Serializable {
        InetAddress serverHost;
        int serverPort;

        public RMIConnectionFactory(String serverHost, int serverPort)
            throws UnknownHostException {
            this(InetAddress.getByName(serverHost), serverPort);
        }

        public RMIConnectionFactory(InetAddress serverHost, int serverPort)
            throws UnknownHostException {
            this.serverHost = serverHost;
            this.serverPort = serverPort;
        }

        public boolean equals(Object obj) {
            RMIConnectionFactory other = (RMIConnectionFactory) obj;

            return other.serverHost.equals(serverHost) && (other.serverPort == serverPort);
        }

        public int hashCode() {
            return serverHost.hashCode() ^ serverPort;
        }

        public String toString() {
            String hostStr = serverHost.getHostName();
            return "rmi:" + hostStr + ":" + serverPort;
        }

        // Let's do our own serialization as InetAddress is not getting
        // serialized correctly
        private void writeObject(ObjectOutputStream out)
            throws IOException {
            out.writeUTF(serverHost.getCanonicalHostName());
            out.writeInt(serverPort);
        }

        private void readObject(ObjectInputStream in) throws IOException, ClassNotFoundException {
            String hostAddress = (String) in.readUTF();
            serverHost = InetAddress.getByName(hostAddress);
            serverPort = (int) in.readInt();
        }
    }

    
    static class ClientSession {
        RMIConnectionFactory factory;
        RMISocket socket;

        public ClientSession(RMIConnectionFactory factory) {
            this.factory = factory;
        }

        public synchronized RMISocket getSocket(ObjectAdapter oa, ClassLoader cl)
            throws IOException {
            if ((socket == null) || socket.isClosed()) {
                Socket sock = new Socket(factory.serverHost, factory.serverPort);
                sock.setTcpNoDelay(true);
                socket = new RMISocket(sock);
                socket.setObjectAdapter(oa);
                socket.setClassLoader(cl);

                RMIServerImpl serverImpl = (RMIServerImpl) oa.getServer();
                ;

                if (serverImpl.getServerThread() == null) {
                    Thread t = new Thread(serverImpl.createListener(socket, oa));
                    t.setDaemon(true);
                    t.start();
                }
            }

            return socket;
        }
    }


    static class RMISocket {
        final int MAX_METHOD_REQUESTS = 128;
        ClassLoader classLoader;
        HashMap indications = new HashMap();
        HashMap requests = new HashMap();
        LinkedList indicationQueue = new LinkedList();
        ObjectAdapter oa;
        ObjectInputStream objIn;
        ObjectOutputStream objOut;
        Socket socket;
        Thread readerThread;
        boolean closed;
        int requestId;

        RMISocket(Socket sock) throws IOException {
            this.socket = sock;
            this.objOut = new ObjectOutputStream(
                    new BufferedOutputStream(sock.getOutputStream(), 8192)
                );
            readerThread = new Thread() {
                        public void run() {
                            runReader();
                        }
                    };
            readerThread.setDaemon(true);
            readerThread.start();
        }

        public String toString() {
            if (socket != null) {
                return socket.toString();
            } else {
                return super.toString();
            }
        }

        public boolean isClosed() {
            return closed || this.socket.isClosed();
        }

        void setClassLoader(ClassLoader classLoader) {
            this.classLoader = classLoader;
        }
        
        void setObjectAdapter(ObjectAdapter oa) {
            this.oa = oa;
        }

        Request createRequest() {
            RequestImpl result;

            synchronized (requests) {
                while (requests.size() > MAX_METHOD_REQUESTS) {
                    try {
                        requests.wait();
                    } catch (InterruptedException exc) {
                        // communication stopped, i.e. detach
                        return null;
                    }
                }

                result = new RequestImpl(requestId++);
                requests.put(new Integer(result.requestId), result);
            }

            return result;
        }

        Indication nextIndication() {
            Indication result;

            synchronized (indicationQueue) {
                while (indicationQueue.size() == 0) {
                    try {
                        indicationQueue.wait();
                    } catch (InterruptedException exc) {
                        // communication stopped, i.e. detach
                        return null;
                    }
                }

                result = (Indication) indicationQueue.removeFirst();

                if (indicationQueue.size() > 0) {
                    indicationQueue.notify();
                }
            }

            return result;
        }

        void runReader() {
            try {
                
                class IStrm extends ObjectInputStream {
                    
                    IStrm(InputStream stream) throws IOException {
                        super(stream);
                        enableResolveObject(true);
                    }

                    public Class resolveClass(ObjectStreamClass osc)
                        throws ClassNotFoundException, IOException {
                        try {
                            if (classLoader != null) {
                                //return classLoader.loadClass(osc.getName());
                                return Class.forName(osc.getName(), true, classLoader);
                            }
                        } catch (ClassNotFoundException exc) {
                            LOGGER.log(Level.WARNING, exc.getMessage());
                        }

                        return super.resolveClass(osc);
                    }

                    protected Class resolveProxyClass(String[] interfaces)
                        throws IOException, ClassNotFoundException {
                        if (classLoader != null) {
                            Class[] classes = new Class[interfaces.length];

                            for (int i = 0; i < classes.length; i++) {
                                //classes[i] = classLoader.loadClass(interfaces[i]);
                                classes[i] = Class.forName(interfaces[i], true, classLoader);
                            }

                            return Proxy.getProxyClass(classLoader, classes);
                        }

                        return super.resolveProxyClass(interfaces);
                    }

                    public Object resolveObject(Object obj) {
                        if (obj instanceof ClientProxy) {
                            ((ClientProxy) obj).setObjectAdapter(oa);
                            ((ClientProxy) obj).setClassLoader(classLoader);
                        } else if (
                            obj instanceof Proxy &&
                                (Proxy.getInvocationHandler(obj) instanceof ClientProxy)
                        ) {
                            ((ClientProxy) Proxy.getInvocationHandler(obj)).setObjectAdapter(oa);
                            ((ClientProxy) Proxy.getInvocationHandler(obj)).setClassLoader(
                                classLoader
                            );
                        }

                        return obj;
                    }
                }
                
                if (socket.isClosed() || closed) {
                    if (oa != null) {
                        oa.notifyClose(socket);
                    }
                    
                    return;
                }
                
                this.objIn = new IStrm(new BufferedInputStream(socket.getInputStream(), 8192));

                while (true) {
                    if (socket.isClosed() || closed) {
                        if (oa != null) {
                            oa.notifyClose(socket);
                        }

                        break;
                    }

                    int t = 0;
                    int reqId = 0;

                    try {
                        t = objIn.readByte();
                        reqId = objIn.readInt();

                        switch (t) {
                        case METHOD_REQ: {
                            if (isDebuggable) {
                                LOGGER.log(Level.FINE, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX");
                                LOGGER.log(
                                    Level.FINE, "RMI socket: " + socket + "  reqID:" + reqId
                                );
                                LOGGER.log(Level.FINE, "Received Request: ");
                                LOGGER.log(Level.FINE, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX");
                            }

                            IndicationImpl ind = new IndicationImpl(reqId);
                            ind.setSocket(this);
                            ind.readIndication();

                            Integer key = new Integer(reqId);

                            synchronized (indicationQueue) {
                                indications.put(key, ind);
                                indicationQueue.addLast(ind);
                                if (indicationQueue.size() == 1) {
                                    indicationQueue.notify();
                                }
                            }
                        }

                        break;

                        case METHOD_RESULT: {
                            if (isDebuggable) {
                                LOGGER.log(Level.FINE, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX");
                                LOGGER.log(
                                    Level.FINE, "RMI socket: " + socket + "  reqID:" + reqId
                                );
                                LOGGER.log(Level.FINE, "Received Result: ");
                                LOGGER.log(Level.FINE, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX");
                            }

                            Integer key = new Integer(reqId);
                            RequestImpl reqImpl = null;

                            synchronized (requests) {
                                reqImpl = (RequestImpl) requests.remove(key);

                                if (requests.size() == (MAX_METHOD_REQUESTS - 1)) {
                                    requests.notify();
                                }
                            }

                            if (reqImpl != null) {
                                reqImpl.readResponse();
                            }
                        }

                        break;

                        case METHOD_EXCEPTION: {
                            if (isDebuggable) {
                                LOGGER.log(Level.FINE, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX");
                                LOGGER.log(
                                    Level.FINE, "RMI socket: " + socket + "  reqID:" + reqId
                                );
                                LOGGER.log(Level.FINE, "Received Exception: ");
                                LOGGER.log(Level.FINE, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX");
                            }

                            Integer key = new Integer(reqId);
                            RequestImpl reqImpl = null;

                            synchronized (requests) {
                                reqImpl = (RequestImpl) requests.remove(key);

                                if (requests.size() == (MAX_METHOD_REQUESTS - 1)) {
                                    requests.notify();
                                }
                            }

                            if (reqImpl != null) {
                                reqImpl.readException();
                            }
                        }

                        break;
                        }
                    } catch (IOException ioexc) {
                        closed = true;
                    }
                }
            } catch (SocketException e) {
                LOGGER.log(Level.WARNING, e.getMessage());
                if (socket.isClosed()) {
                    closed = true;
                }
            } catch (ClassNotFoundException exc) {
                LOGGER.log(Level.WARNING, exc.getMessage());
                closed = true;
            } catch (IOException exc) {
                LOGGER.log(Level.WARNING, exc.getMessage());
            }
        }

        
        class IndicationImpl implements Indication {
            RMISocket rmiSocket;
            String adapterName;
            String methodName;
            String methodSig;
            String objectKey;
            Object[] arguments;
            int requestId;

            IndicationImpl(int reqId) {
                this.requestId = reqId;
            }

            public String getAdapterName() {
                return adapterName;
            }

            public Object[] getArguments() {
                return arguments;
            }

            public String getMethodName() {
                return methodName;
            }

            public String getMethodSignature() {
                return methodSig;
            }

            public String getObjectKey() {
                return objectKey;
            }

            public void raiseException(Throwable exc) throws IOException {
                
                synchronized (indicationQueue) {
                    indications.remove(new Integer(requestId));
                }

                synchronized (objOut) {
                    objOut.reset();
                    objOut.writeByte(METHOD_EXCEPTION);
                    objOut.writeInt(requestId);
                    objOut.writeObject(exc);
                    objOut.flush();
                }
            }

            public void sendResponse(Object result) throws IOException {
                synchronized (indicationQueue) {
                    indications.remove(new Integer(requestId));
                }

                if (isDebuggable) {
                    LOGGER.log(Level.FINE, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX");
                    LOGGER.log(Level.FINE, "Sending response :");
                    LOGGER.log(Level.FINE, "requstid:" + requestId);
                    LOGGER.log(Level.FINE, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX");
                }

                synchronized (objOut) {
                    objOut.reset();
                    objOut.writeByte(METHOD_RESULT);
                    objOut.writeInt(requestId);
                    objOut.writeObject(result);
                    objOut.flush();
                }
            }

            void readIndication() throws IOException, ClassNotFoundException {
                adapterName = (String) objIn.readUTF();
                objectKey = (String) objIn.readUTF();
                methodName = (String) objIn.readUTF();

                if (isDebuggable) {
                    LOGGER.log(Level.FINE, "method name :" + methodName);
                    LOGGER.log(Level.FINE, "object key :" + objectKey);
                }

                methodSig = (String) objIn.readUTF();

                int len = objIn.readUnsignedByte();
                arguments = new Object[len];

                for (int i = 0; i < len; i++) {
                    arguments[i] = (Object) objIn.readObject();

                    if ((arguments[i] != null) && arguments[i] instanceof Proxy) {
                        ((ClientProxy) (Proxy.getInvocationHandler(arguments[i]))).socket = rmiSocket;
                    } else if ((arguments[i] != null) && isDebuggable) {
                        LOGGER.log(Level.FINE, "arguments[" + "i]" + arguments[i]);
                    }
                }
            }

            public void setSocket(RMISocket socket) {
                this.rmiSocket = socket;
            }
        }

        
        class RequestImpl implements Request {
            Method method;
            Object monitor = new Object();
            Object result;
            String adapterName;
            String objectKey;
            Object[] arguments;
            int requestId;
            int resultFlag;

            RequestImpl(int requestId) {
                this.requestId = requestId;
            }

            public void setAdapterName(String adapterName) {
                this.adapterName = adapterName;
            }

            public void setArguments(Object[] arguments) {
                this.arguments = arguments;
            }

            public void setMethod(Method method) {
                this.method = method;
            }

            public void setObjectKey(String objectKey) {
                this.objectKey = objectKey;
            }

            public Object getResult() throws Throwable {
                if (isDebuggable) {
                    LOGGER.log(Level.FINE, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX");
                    LOGGER.log(Level.FINE, "Sending request :");
                    LOGGER.log(Level.FINE, "requstId: " + requestId);
                    LOGGER.log(Level.FINE, "method name: " + method.getName());
                    LOGGER.log(Level.FINE, "method name: " + method.getName());
                    LOGGER.log(Level.FINE, "object key: " + objectKey);
                    LOGGER.log(Level.FINE, "sending to: " + socket);
                    LOGGER.log(Level.FINE, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX");
                }

                resultFlag = METHOD_REQ;

                synchronized (objOut) {
                    objOut.reset();
                    objOut.writeByte(METHOD_REQ);
                    objOut.writeInt(requestId);
                    objOut.writeUTF(adapterName);
                    objOut.writeUTF(objectKey);
                    objOut.writeUTF(method.getName());
                    objOut.writeUTF(getParameterTypes(method));

                    int len = (arguments == null) ? 0 : arguments.length;
                    objOut.writeByte(len);

                    for (int i = 0; i < len; i++) {
                        objOut.writeObject(arguments[i]);
                    }

                    objOut.flush();
                }

                synchronized (monitor) {
                    while (resultFlag == METHOD_REQ) {
                        monitor.wait();
                    }
                }

                if (resultFlag == METHOD_EXCEPTION) {
                    throw (Throwable) result;
                }

                return result;
            }

            void readException() throws IOException, ClassNotFoundException {
                result = objIn.readObject();

                synchronized (monitor) {
                    resultFlag = METHOD_EXCEPTION;
                    monitor.notify();
                }
            }

            void readResponse() throws IOException, ClassNotFoundException {
                result = objIn.readObject();

                if (result instanceof Proxy) {
                    ((ClientProxy) (Proxy.getInvocationHandler(result))).socket = RMISocket.this;
                }

                synchronized (monitor) {
                    resultFlag = METHOD_RESULT;
                    monitor.notify();
                }
            }
        }
    }


    static class TransientRef extends WeakReference {
        final String key;

        public TransientRef(String key, Object obj, ReferenceQueue queue) {
            super(obj, queue);
            this.key = key;
        }
    }


    class RMIClientImpl implements RMIClient {
        ObjectAdapter oa;
        RMIConnectionFactory factory;

        RMIClientImpl(InetAddress serverHost, int serverPort)
            throws IOException {
            this.factory = new RMIConnectionFactory(serverHost, serverPort);
        }

        public void setObjectAdapter(ObjectAdapter oa) {
            this.oa = oa;

            if (oa != null) {
                oa.setClient(this);
            }
        }

        public InetAddress getRemoteAddress() {
            return factory.serverHost;
        }

        public int getRemotePort() {
            return factory.serverPort;
        }

        public RMIService getService() {
            return DefaultRMIService.this;
        }

        public void destroy() {
        }

        public Object importObject(String adapterName, String objectKey)
            throws IOException {
            return null;
        }

        public Object importObject(Class clazz, String adapterName, String objectKey)
            throws IOException {
            Class[] ifaces = clazz.getInterfaces();

            if (clazz.isInterface()) {
                Class[] arr = new Class[ifaces.length + 1];
                System.arraycopy(ifaces, 0, arr, 1, ifaces.length);
                arr[0] = clazz;
                ifaces = arr;
            }

            return importObject(clazz.getClassLoader(), ifaces, adapterName, objectKey);
        }

        public Object importObject(
                ClassLoader classLoader, 
                Class[] interfaces, 
                String adapterName, 
                String objectKey
        ) throws IOException {
            
            Object localRef = null;

            synchronized (servers) {
                RMIServer server = (RMIServer) servers.get(factory);

                if (server != null) {
                    // collocation optimization
                    ObjectAdapter adapter = server.createObjectAdapter(adapterName);
                    localRef = adapter.getObjectWithKey(objectKey);
                }
            }

            Object result = createProxy(classLoader, interfaces, adapterName, objectKey, localRef);

            return result;
        }

        private Object createProxy(
                ClassLoader classLoader, 
                Class[] interfaces, 
                String queueName, 
                String objectKey,
            Object localRef
        ) throws IOException {
            
            ClientProxy clientProxy = new ClientProxy(factory, queueName, objectKey, localRef, oa);
            clientProxy.setClassLoader(classLoader);

            Object result = Proxy.newProxyInstance(classLoader, interfaces, clientProxy);

            return result;
        }
    }

    
    class RMIServerImpl implements RMIServer {
        HashMap adapters = new HashMap();
        ObjectAdapter defaultObjectAdapter;
        RMIClient cli;
        RMIConnectionFactory factory;
        Server serverThread = null;

        public RMIServerImpl(InetAddress hostName, int port)
            throws UnknownHostException, IOException {
            if (isDebuggable) {
                LOGGER.log(Level.FINE, "Creating RMIServerImpl");
            }

            factory = new RMIConnectionFactory(hostName, port);
            serverThread = new Server(hostName, port);
        }

        public RMIServerImpl() throws UnknownHostException, IOException {
            InetAddress inetAddr = InetAddress.getLocalHost();
            ServerSocket sock = new ServerSocket(0, 0, inetAddr);
            factory = new RMIConnectionFactory(sock.getInetAddress(), sock.getLocalPort());

            synchronized (servers) {
                servers.put(factory, this);
            }

            serverThread = null;
        }

        public Server getServerThread() {
            return serverThread;
        }

        public RMIClient getClient() throws IOException {
            if (cli == null) {
                cli = DefaultRMIService.this.createClient(factory.serverHost, factory.serverPort);
            }

            return cli;
        }

        public void setDefaultAdaptor(ObjectAdapter oa) {
            defaultObjectAdapter = oa;
        }

        public InetAddress getLocalAddress() {
            return factory.serverHost;
        }

        public int getLocalPort() {
            return factory.serverPort;
        }

        public RMIService getService() {
            return DefaultRMIService.this;
        }

        public RMIClient createClient(String host, int port)
            throws IOException {
            return createClient(InetAddress.getByName(host), port);
        }

        public RMIClient createClient(InetAddress host, int port)
            throws IOException {
            RMIClientImpl result = new RMIClientImpl(host, port);
            result.setObjectAdapter(defaultObjectAdapter);

            return result;
        }

        public ObjectAdapter createObjectAdapter(String adapterName)
            throws IOException {
            ObjectAdapterImpl result = (ObjectAdapterImpl) adapters.get(adapterName);

            if (result == null) {
                result = new ObjectAdapterImpl(adapterName);

                synchronized (adapters) {
                    adapters.put(adapterName, result);
                }
            }

            return result;
        }

        public void closeClients() {
            if (isDebuggable) {
                LOGGER.log(Level.FINE, "Closing client connections ...");
            }
            if (serverThread != null) {
                serverThread.closeClientConnection();
            }
            synchronized (clientSessionPool) {
                clientSessionPool.clear();
            }            
        }

        public void destroy() {
            if (isDebuggable) {
                LOGGER.log(Level.FINE, "destroy RMIServerImpl = " + this);
            }

            synchronized (servers) {
                servers.remove(factory);
            }
            if (serverThread != null) {
                serverThread.closeClientConnection();
                serverThread.closeServerConnection();
                serverThread.interrupt();
            }
        }

        public void run() {
            if (serverThread != null) {
                serverThread.setDaemon(true);
                serverThread.start();
            }

            synchronized (adapters) {
                while (alive && (adapters.size() > 0)) {
                    try {
                        adapters.wait();
                    } catch (InterruptedException exc) {
                        // communication stopped, i.e. detach
                        return;
                    }
                }
            }
        }

        public SocketResponseListener createListener(RMISocket sock, ObjectAdapter adapter) {
            return new SocketResponseListener(sock, adapter);
        }

        
        class SocketResponseListener implements Runnable {
            RMISocket sock;
            ObjectAdapter adapter;

            SocketResponseListener(RMISocket sock, ObjectAdapter adapter) {
                this.sock = sock;
                this.adapter = adapter;
            }

            public void run() {
                try {
                    while (alive) {
                        Indication ind = sock.nextIndication();
                        ((ObjectAdapterImpl) adapter).indicate(ind);
                    }
                } catch (Exception exc) {
                    LOGGER.log(Level.WARNING, exc.getMessage(), exc);
                }
            }
        }

        
        public class ObjectAdapterImpl implements ObjectAdapter {
            DebugListener[] listeners = new DebugListener[1];
            HashMap servants = new HashMap();
            LinkedList indicationQueue = new LinkedList();
            LinkedList invokers = new LinkedList();
            RMIClient cli;
            ReferenceQueue transientRefQueue = new ReferenceQueue();
            String adapterName;
            Thread mThread;

            public ObjectAdapterImpl(String adapterName)
                throws IOException {
                this.adapterName = adapterName;
                cli = (RMIClientImpl) getServer().getClient();
                cli.setObjectAdapter(this);
            }

            public String getAdapterName() {
                return adapterName;
            }

            public synchronized Object getObject(String objectKey) {
                try {
                    return getObjectWithKey(objectKey);
                } catch (Exception exc) {
                    LOGGER.log(Level.WARNING, exc.getMessage(), exc);
                }

                return null;
            }

            public String[] getObjectKeys(Object servant) {
                List result = new LinkedList();

                synchronized (servants) {
                    Iterator iter = servants.entrySet().iterator();

                    while (iter.hasNext()) {
                        Map.Entry e = (Map.Entry) iter.next();
                        Object value = e.getValue();

                        if (value instanceof TransientRef) {
                            value = ((TransientRef) value).get();
                        }

                        if (value == servant) {
                            result.add(e.getKey());
                        }
                    }
                }

                String[] arr = new String[result.size()];
                result.toArray(arr);

                return arr;
            }

            public Object getObjectWithKey(String objectKey)
                throws IOException {
                Object res = findServant(objectKey);

                if (res == null) {
                    throw new IOException(rb.getString("STR_NO_SUCH_OBJ") + ": " + objectKey);
                }

                return res;
            }

            public RMIServer getServer() {
                return RMIServerImpl.this;
            }

            public Invoker createInvoker(
                    final Object target, 
                    final Method method, 
                    final Indication ind
            ) {
                
                InvokerImpl inv;

                synchronized (invokers) {
                    if (invokers.size() == 0) {
                        inv = new InvokerImpl();
                    } else {
                        inv = (InvokerImpl) invokers.removeFirst();
                    }
                }

                final InvokerImpl finalInv = inv;
                inv.init(target, method, ind);

                return new Invoker() {
                        public void start() {
                            threadPool.execute(finalInv);
                        }
                    };
            }

            public void destroy() {
                mThread.interrupt();
            }

            public Object exportObject(Object servant)
                throws IOException {
                String key = "" + System.identityHashCode(servant);

                return exportObject0(key, servant, servant);
            }

            public Object exportObject(String objectKey, Object servant)
                throws IOException {
                return exportObject0(objectKey, servant, servant);
            }

            public void indicate(Indication ind) {
                synchronized (indicationQueue) {
                    indicationQueue.addLast(ind);

                    if (indicationQueue.size() == 1) {
                        indicationQueue.notify();
                    }
                }
            }

            public void run() {
                try {
                    while (alive) {
                        pollRefs();

                        Indication ind;
                        synchronized (indicationQueue) {
                            while (alive && (indicationQueue.size() == 0)) {
                                indicationQueue.wait();
                            }
                            ind = (Indication) indicationQueue.removeFirst();
                        }

                        try {
                            String objectKey = ind.getObjectKey();
                            Object target = findServant(objectKey);

                            if (target == null) {
                                ind.raiseException(
                                    new RuntimeException(
                                        rb.getString("STR_NO_SUCH_OBJ") + ": " + factory + "/" +
                                        "/" + adapterName + "#" + objectKey
                                    )
                                );

                                continue;
                            }

                            Method method = findMethod(
                                    target.getClass(), ind.getMethodName(), ind.getMethodSignature()
                                );

                            createInvoker(target, method, ind).start();
                        } catch (RejectedExecutionException e) {
                            LOGGER.log(Level.WARNING, e.getMessage());
                        } catch (Throwable t) {
                            LOGGER.log(Level.WARNING, t.getMessage());
                            ind.raiseException(t);
                        }
                    }
                    
                } catch (InterruptedException exc) {
                    // communication stopped, i.e. detach
                    synchronized (adapters) {
                        adapters.remove(adapterName);
                        if (adapters.size() == 0) {
                            adapters.notify();
                        }
                    }
                } catch (Exception exc) {
                    LOGGER.log(Level.WARNING, exc.getMessage());
                    synchronized (adapters) {
                        adapters.remove(adapterName);
                        if (adapters.size() == 0) {
                            adapters.notify();
                        }
                    }
                }
            }

            public void start() {
                mThread = new Thread(this);
                mThread.setDaemon(true);
                mThread.start();
            }

            public String[] unexportObject(Object servant) {
                return removeServant(servant);
            }

            public Object unexportObjectWithKey(String objectKey) {
                return removeServantWithKey(objectKey);
            }

            void pollRefs() {
                TransientRef ref;
                while ((ref = (TransientRef) transientRefQueue.poll()) != null) {
                    removeServantWithKey(ref.key);
                }
            }

            private void addServant(String key, Object obj) {
                synchronized (servants) {
                    servants.put(key, obj);
                }
            }

            private Object exportObject0(String objectKey, Object servant, Object ref)
                throws IOException {
                addServant(objectKey, ref);

                return ((RMIClientImpl) cli).createProxy(
                    servant.getClass().getClassLoader(), servant.getClass().getInterfaces(),
                    adapterName, objectKey, servant
                );
            }

            private Object findServant(String key) {
                synchronized (servants) {
                    Object result = servants.get(key);

                    if ((result != null) && (result instanceof TransientRef)) {
                        TransientRef ref = (TransientRef) result;
                        result = ref.get();

                        if (result == null) {
                            servants.remove(key);
                        }
                    }

                    return result;
                }
            }

            private String[] removeServant(Object servant) {
                LinkedList result = new LinkedList();

                synchronized (servants) {
                    Iterator iter = servants.entrySet().iterator();

                    while (iter.hasNext()) {
                        Map.Entry e = (Map.Entry) iter.next();
                        Object value = e.getValue();

                        if (value instanceof TransientRef) {
                            value = ((TransientRef) value).get();
                        }

                        if (value == servant) {
                            String key = (String) e.getKey();
                            result.add(key);
                            iter.remove();
                        }
                    }
                }

                String[] arr = new String[result.size()];
                result.toArray(arr);

                return arr;
            }

            private Object removeServantWithKey(String key) {
                synchronized (servants) {
                    return servants.remove(key);
                }
            }

            public void setClient(RMIClient client) {
                cli = client;
            }

            public void registerListerner(DebugListener listener) {
                listeners[0] = listener;
            }

            public void notifyClose(Object socket) {
                LOGGER.log(Level.FINE, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX");
                LOGGER.log(Level.FINE, "Socket: " + socket + " is closed");
                LOGGER.log(Level.FINE, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX");

                if (listeners.length > 0) {
                    listeners[0].socketClosed(socket);
                }
            }

            
            public class InvokerImpl implements Runnable {
                Indication ind;
                Method method;
                Object target;

                public void run() {
                    Object[] args = ind.getArguments();

                    try {
                        try {
                            method.setAccessible(true);

                            Object result = method.invoke(target, args);

                            if (
                                (result != null) && !(result instanceof Proxy) &&
                                    !(result instanceof String) && !(result instanceof Number) &&
                                    !(result instanceof Boolean) && !(isPrimitiveArray(result))
                            ) {
                                result = exportObject(result);
                            }

                            ind.sendResponse(result);
                        } catch (InvocationTargetException exc) {
                            ind.raiseException(exc.getTargetException());
                        } catch (Throwable t) {
                            ind.raiseException(t);
                        }
                    } catch (IOException exc) {
                        LOGGER.log(Level.WARNING, exc.getMessage());
                    }
                }

                void init(Object target, Method method, Indication ind) {
                    this.target = target;
                    this.method = method;
                    this.ind = ind;
                }

                void release() {
                    ind = null;
                    target = null;

                    synchronized (invokers) {
                        invokers.addLast(this);
                    }
                }
            }
        }


        class ClientWorker implements Runnable {
            RMISocket sock;

            ClientWorker(RMISocket sock) {
                this.sock = sock;
            }

            public void run() {
                try {
                    while (alive) {
                        Indication ind = sock.nextIndication();

                        if (ind == null) {
                            continue;
                        }

                        ObjectAdapterImpl adapter;

                        synchronized (adapters) {
                            adapter = (ObjectAdapterImpl) adapters.get(ind.getAdapterName());

                            if (adapter == null) {
                                ind.raiseException(
                                    new RuntimeException(
                                        rb.getString("STR_UNKNOWN_ADAPT") + ": " + factory + "/" +
                                        ind.getAdapterName()
                                    )
                                );

                                continue;
                            }
                        }

                        adapter.indicate(ind);
                    }
                } catch (Exception exc) {
                    LOGGER.log(Level.WARNING, exc.getMessage());
                }
            }
        }


        class Server extends Thread {
            ServerSocket serverSock;
            Vector clients = new Vector();
            Vector opennedSocks = new Vector();

            Server(ServerSocket sock) {
                this.serverSock = sock;
            }

            Server(InetAddress address, int port) {
                try {
                    serverSock = new ServerSocket(port, 1000, address);
                } catch (IOException e) {
                    LOGGER.log(Level.WARNING, e.getMessage());
                    throw new RuntimeException(
                        "Unable to open socket: host--" + address + " port--" + port +
                        ". You might have more than one project deployed in debug mode!"
                    );
                }
            }

            Server(String host, int port) throws IOException {
                this(InetAddress.getByName(host), port);
            }

            public synchronized void closeClientConnection() {
                Vector clientsCopy = new Vector (clients);
                for (Iterator it = clientsCopy.iterator(); it.hasNext();) {
                    Thread t = (Thread) it.next();
                    t.interrupt();
                }
                Vector opennedSocksCopy = new Vector (opennedSocks);
                for (Iterator it = opennedSocksCopy.iterator(); it.hasNext();) {
                    Socket s = (Socket) it.next();

                    try {
                        s.close();
                    } catch (Exception e) {
                        LOGGER.log(Level.WARNING, e.getMessage());
                    }
                }

                clients.clear();
                opennedSocks.clear();
            }

            public void closeServerConnection() {
                if (serverSock != null) {
                    try {
                        serverSock.close();
                    } catch (IOException e) {
                        LOGGER.log(Level.WARNING, e.getMessage());
                    } finally {
                        serverSock = null;
                    }
                }
            }

            public void run() {
                if (isDebuggable) {
                    LOGGER.log(Level.INFO, "listening to socket =" + serverSock);
                }

                try {
                    while (alive) {
                        Socket sock = null;
                        sock = serverSock.accept();
                        opennedSocks.add(sock);
                        sock.setTcpNoDelay(true);

                        RMISocket rmiSock = new RMISocket(sock);
                        rmiSock.setObjectAdapter(defaultObjectAdapter);
                        rmiSock.setClassLoader(DefaultRMIService.this.classLoader);

                        Thread clientThread = new Thread(new ClientWorker(rmiSock));
                        clients.add(clientThread);
                        clientThread.setDaemon(true);
                        clientThread.start();
                    }
                } catch (java.net.SocketException e) {
                    LOGGER.log(Level.WARNING, e.getMessage());
                } catch (Exception exc) {
                    LOGGER.log(Level.WARNING, exc.getMessage());
                }

                if (isDebuggable) {
                    LOGGER.log(Level.FINE, "ending server thread = " + this);
                }
            }
        }
    }
}
