package it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany;


/**
* it/imolinfo/jbi4corba/test/webservice/generator/echocomplex/typedefany/_EchoComplexTypeDefAnyStub.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from EchoTypeDefAny.idl
* mercoledý 10 febbraio 2010 15.46.41 CET
*/


//==================================================
public class _EchoComplexTypeDefAnyStub extends org.omg.CORBA.portable.ObjectImpl implements it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.typedefany.EchoComplexTypeDefAny
{

  public org.omg.CORBA.Any echo (org.omg.CORBA.Any msg)
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("echo", true);
                $out.write_any (msg);
                $in = _invoke ($out);
                org.omg.CORBA.Any $result = $in.read_any ();
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return echo (msg        );
            } finally {
                _releaseReply ($in);
            }
  } // echo

  public String echoinout (org.omg.CORBA.AnyHolder msg)
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("echoinout", true);
                $out.write_any (msg.value);
                $in = _invoke ($out);
                String $result = $in.read_string ();
                msg.value = $in.read_any ();
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return echoinout (msg        );
            } finally {
                _releaseReply ($in);
            }
  } // echoinout

  // Type-specific CORBA::Object operations
  private static String[] __ids = {
    "IDL:it/imolinfo/jbi4corba/test/webservice/generator/echocomplex/typedefany/EchoComplexTypeDefAny:1.0"};

  public String[] _ids ()
  {
    return (String[])__ids.clone ();
  }

  private void readObject (java.io.ObjectInputStream s) throws java.io.IOException
  {
     String str = s.readUTF ();
     String[] args = null;
     java.util.Properties props = null;
     org.omg.CORBA.Object obj = org.omg.CORBA.ORB.init (args, props).string_to_object (str);
     org.omg.CORBA.portable.Delegate delegate = ((org.omg.CORBA.portable.ObjectImpl) obj)._get_delegate ();
     _set_delegate (delegate);
  }

  private void writeObject (java.io.ObjectOutputStream s) throws java.io.IOException
  {
     String[] args = null;
     java.util.Properties props = null;
     String str = org.omg.CORBA.ORB.init (args, props).object_to_string (this);
     s.writeUTF (str);
  }
} // class _EchoComplexTypeDefAnyStub
