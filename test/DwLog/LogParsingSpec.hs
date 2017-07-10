{-# LANGUAGE OverloadedStrings #-}
module DwLog.LogParsingSpec (main, spec) where

import           Control.Monad
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import qualified Data.Text.Lazy.IO   as LTIO
import           Data.Time
import           System.Directory
import           Test.Hspec
import           Text.Parsec
import           Text.Parsec.Text

import           DwLog
import           DwLog.LogParsing
import           DwLog.LogParsing.IO

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Custom log entries" $ do
    assertLogValues
      (customLogParser $ DwLogMeta dummyClient "file1" "some-instance")
      "test-logs/customerror-blade0-2-appserver-20160608.log"
      [ [ Timestamp $ LocalTime (fromGregorian 2016 6 8) (TimeOfDay 0 41 23.325)
        , Client dummyClient
        , Filename "file1"
        , LogTypeField CustomLog
        , Level "ERROR"
        , Servlet "PipelineCallServlet"
        , Site "mysite03_us"
        , Pipeline "ProDeal-Show"
        , Caller "PipelineCall"
        , SessionID "jgfvcEJOmeoHTQZTsWyweUZMtXgvDTuspLl4fxNrSc9LVXZRG4lZdaf0zEkhK2YEIDzwNpWJk55Z7N4QERvdqQ=="
        , SessionType Nothing
        , RequestID Nothing
        , Logger "custom"
        , Message
          "Error executing script 'account/ValidateMailingAddressFields.ds' (pipelineName=Account):\n\
          \Script exception in line 15:org.mozilla.javascript.EcmaError: TypeError: Cannot find function \
          \getSortedFormFields in object \n\
          \function DynamicAddressForm() {...}. (www_store:account/ValidateMailingAddressFields.ds#15)"
        , SystemInfo []
        , RequestInfo []
        , RequestParams []
        , MalformedRequestParams Nothing
        , StrayHttpHeaders Nothing
        , StackTraceField Nothing
        , JobName Nothing
        , QuotaInfoField Nothing
        , DeprecationInfoField Nothing
        ]
      , [ Timestamp $ LocalTime (fromGregorian 2016 6 8) (TimeOfDay 0 42 21.194)
        , Client dummyClient
        , Filename "file1"
        , LogTypeField CustomLog
        , Level "ERROR"
        , Servlet "PipelineCallServlet"
        , Site "mysite02_us"
        , Pipeline "Blog-Show"
        , Caller "PipelineCall"
        , SessionID "FVGRIiGVfR1ewE6lWHrFmCLQtLBI1yGamLr69oA7_5Q8inPMfxVZgB9F7sj6kHV1z6et3Qv5Nsr39NyrdYfdXA=="
        , SessionType Nothing
        , RequestID Nothing
        , Logger "custom"
        , Message
          "TypeError: Cannot call method \"split\" of [something almost timestampy] undefined\n\
          \and now an actual timestamp [2016-06-08 00:41:23.325 GMT] mid-message"
        , SystemInfo []
        , RequestInfo []
        , RequestParams []
        , MalformedRequestParams Nothing
        , StrayHttpHeaders Nothing
        , StackTraceField Nothing
        , JobName Nothing
        , QuotaInfoField Nothing
        , DeprecationInfoField Nothing
        ]
      , [ Timestamp $ LocalTime (fromGregorian 2016 6 6) (TimeOfDay 13 0 38.385)
        , Client dummyClient
        , Filename "file1"
        , LogTypeField CustomLog
        , Level "ERROR"
        , Servlet "JobThread"
        , Site ""
        , Pipeline "Export-Orders"
        , Caller ""
        , SessionID ""
        , SessionType Nothing
        , RequestID Nothing
        , Logger "custom"
        , Message
          "WebserviceConfiguration.ds: no Webservice credential object found - fall back to system unavailable"
        , SystemInfo []
        , RequestInfo []
        , RequestParams []
        , MalformedRequestParams Nothing
        , StrayHttpHeaders Nothing
        , StackTraceField Nothing
        , JobName $ Just "Export - Orders"
        , QuotaInfoField Nothing
        , DeprecationInfoField Nothing
        ]
      ]

  describe "DW error log entries" $ do
    assertLogValues
      (dwSystemLogParser $ DwLogMeta dummyClient "file2" "some-instance")
      "test-logs/error-blade0-2-appserver-20160608.log"
      [ [ Timestamp $ LocalTime (fromGregorian 2016 6 8) (TimeOfDay 0 1 14.283)
        , Client dummyClient
        , Filename "file2"
        , LogTypeField SystemLog
        , Level "ERROR"
        , Servlet "PipelineCallServlet"
        , Site "mysite07_us"
        , Pipeline "Blog-Show"
        , Caller "PipelineCall"
        , SessionID "UYpyajuCXp7g_kQKJ6yeT2LQeC6RjX5vfqZLCNMg3i5umezcPxWcur8XBzSF1X0LItwguMXndCc1TsZBNYcYZg=="
        , SessionType $ Just "STOREFRONT"
        , RequestID $ Just "oTUOLFdXYEjIAgAK-0-00"
        , Logger "system.TemplateScriptError"
        , Message
          "TypeError: Cannot read property \"products\" from null ([Template:search/components/categorycarousel:${cat.products}]#1)\n\
          \\tat [Template:search/components/categorycarousel:${cat.products}]:1"
        , SystemInfo
          [ ("RequestID", "oTUOLFdXYEjIAgAK-0-00")
          , ("SessionType", "STOREFRONT")
          , ("SessionID", "UYpyajuCXp7g_kQKJ6yeT2LQeC6RjX5vfqZLCNMg3i5umezcPxWcur8XBzSF1X0LItwguMXndCc1TsZBNYcYZg==")
          , ("ServerName", "localhost")
          , ("ServerPort", "10163")
          ]
        , RequestInfo
          [ ("URI", "/servlet/Beehive/Sites-mysite07_us-Site/default/Blog-Show")
          , ("Method", "GET")
          , ("PathInfo", "/Sites-mysite07_us-Site/default/Blog-Show")
          , ("QueryString", "url=%2Fmysite07_us%2Fproducts%2Fbehind-hike%2F")
          , ("Remote Address", "157.55.39.182")
          ]
        , RequestParams [("url", "/mysite07_us/products/behind-hike/")]
        , MalformedRequestParams Nothing
        , StrayHttpHeaders Nothing
        , StackTraceField $ Just $ StackTrace "f727e7a9b74859a6b01d4df7a3f10da6"
          "org.mozilla.javascript.WrappedException: Wrapped com.demandware.beehive.core.capi.common.NullArgumentException: Null Arguments in ProductEvaluator.ctor(Domain, Product, ProductLineItem) ([Template:product/components/promotions:${dw.campaign.PromotionMgr.activeCustomerPromotions.getProductPromotions(pdict.promo_product)}]#1)\n\
          \\tat org.mozilla.javascript.Context.throwAsScriptRuntimeEx(Context.java:1754)\n\
          \\tat org.mozilla.javascript.MemberBox.invoke(MemberBox.java:233)\n\
          \\tat org.mozilla.javascript.NativeJavaMethod.call(NativeJavaMethod.java:250)\n\
          \Caused by: com.demandware.beehive.core.capi.common.NullArgumentException: Null Arguments in ProductEvaluator.ctor(Domain, Product, ProductLineItem)\n\
          \\tat com.demandware.beehive.core.capi.common.NullArgumentException.assertArgs(NullArgumentException.java:48)\n\
          \\tat com.demandware.component.transaction.promotion.spec.ProductEvaluator.<init>(ProductEvaluator.java:120)\n\
          \\t... 176 more"
        , JobName Nothing
        , QuotaInfoField Nothing
        , DeprecationInfoField Nothing
        ]

      , [ Timestamp $ LocalTime (fromGregorian 2016 6 8) (TimeOfDay 0 1 37.804)
        , Client dummyClient
        , Filename "file2"
        , LogTypeField SystemLog
        , Level "ERROR"
        , Servlet "PipelineCallServlet"
        , Site "mysite09_us"
        , Pipeline "Search-Show"
        , Caller "OnRequest"
        , SessionID "XFUYk4-LSJBgtS82x1YFmsLlrFGZ8OFQ9JdGIlhHYlKrPNFZnyBI7CqvvrDrwzbP51JoSW0z0Q8sfpkolbLMMA=="
        , SessionType $ Just "STOREFRONT"
        , RequestID $ Just "AjYTLVdXYGHIAgAK-0-00"
        , Logger "system.core"
        , Message
          "An infinite loop was detected within the Redirect URL configuration. Caused by request URL \
          \/Sites-mysite09_us-Site/default/Search-Show?cgid=discover-nyc-pop-up."
        , SystemInfo
          [ ("RequestID", "AjYTLVdXYGHIAgAK-0-00")
          , ("SessionType", "STOREFRONT")
          , ("SessionID", "XFUYk4-LSJBgtS82x1YFmsLlrFGZ8OFQ9JdGIlhHYlKrPNFZnyBI7CqvvrDrwzbP51JoSW0z0Q8sfpkolbLMMA==")
          , ("ServerName", "localhost")
          , ("ServerPort", "10163")
          ]
        , RequestInfo
          [ ("URI", "/servlet/Beehive/Sites-mysite09_us-Site/default/Search-Show")
          , ("Method", "GET")
          , ("PathInfo", "/Sites-mysite09_us-Site/default/Search-Show")
          , ("QueryString", "cgid=discover-nyc-pop-up")
          , ("Remote Address", "52.90.132.142")
          ]
        , RequestParams
          [ ("cgid", "discover-nyc-pop-up")
          , ("dwfrm_billing_billingAddress_addressFields_firstName", "Nina")
          , ("malformed",
              "Seriously? You just\n\
              \dump multiline query parameters\n\
              \straight to the log file without any\n\
              \good serialization. Thanks.")
          , ("shipping-address1", "410 Miracle Mile")
          ]
        , MalformedRequestParams Nothing
        , StrayHttpHeaders $ Just
          "POST /on/demandware.store/Sites-mysite10_us-Site/default/Cart-AddProduct?format=ajax HTTP/1.1\n\
          \Host: www.mysite10.com\n\
          \Connection: keep-alive\n\
          \Content-Length: 174\n\
          \Accept: */*\n\
          \O"
        , StackTraceField $ Just $ StackTraceRef "f727e7a9b74859a6b01d4df7a3f10da6"
        , JobName Nothing
        , QuotaInfoField Nothing
        , DeprecationInfoField Nothing
        ]

      , [ Timestamp $ LocalTime (fromGregorian 2016 6 8) (TimeOfDay 0 30 17.329)
        , Client dummyClient
        , Filename "file2"
        , LogTypeField SystemLog
        , Level "ERROR"
        , Servlet "PipelineCallServlet"
        , Site "mysite11_us"
        , Pipeline "__Analytics-Tracking"
        , Caller "OnRequest"
        , SessionID "1YxaShMLgyQMklwt9D9Mr558TJwOQqHVNhkQ2Hap_iZDik9eV-6JdbiHNCqB2tVxGTmXb0TyVHycE9f1ySsriQ=="
        , SessionType $ Just "STOREFRONT"
        , RequestID $ Just "mDVZ5VdXZxnIAgAK-0-00"
        , Logger "system"
        , Message
          "Cannot process a request parameter because it has multiple values. Instead will \
          \use the default value [{}]. Parameter [{}]. Values [{}]. Default value [{}]"
        , SystemInfo []
        , RequestInfo []
        , RequestParams []
        , MalformedRequestParams Nothing
        , StrayHttpHeaders Nothing
        , StackTraceField Nothing
        , JobName Nothing
        , QuotaInfoField Nothing
        , DeprecationInfoField Nothing
        ]

      , [ Timestamp $ LocalTime (fromGregorian 2016 6 8) (TimeOfDay 0 1 38.373)
        , Client dummyClient
        , Filename "file2"
        , LogTypeField SystemLog
        , Level "ERROR"
        , Servlet "PipelineCallServlet"
        , Site "mysite11_us"
        , Pipeline "Product-Detail"
        , Caller "PipelineCall"
        , SessionID "aE_qQrA_Co47bU0fzgfLjteXI-TKha_jYcmikq3doOYy2wnrilLHWqi6KjZgj57TcZSZAGFOkvbJ8JkjgMBDhg=="
        , SessionType $ Just "STOREFRONT"
        , RequestID $ Just "XDcqLVdXYGLIAgAK-0-00"
        , Logger "system.TemplateScriptError"
        , Message
          "TypeError: Cannot read property \"onlineFrom\" from null ( dw.util );\n ... }]#8)\n\
          \\tat [Template:product/productpagedynamiccache:${importPackage( dw.util );\n ... }]:8"
        , SystemInfo
          [ ("RequestID", "XDcqLVdXYGLIAgAK-0-00")
          , ("SessionType", "STOREFRONT")
          , ("SessionID", "aE_qQrA_Co47bU0fzgfLjteXI-TKha_jYcmikq3doOYy2wnrilLHWqi6KjZgj57TcZSZAGFOkvbJ8JkjgMBDhg==")
          , ("ServerName", "localhost")
          , ("ServerPort", "10163")
          ]
        , RequestInfo
          [ ("URI", "/servlet/Beehive/Sites-mysite11_us-Site/default/Product-Detail/C1793844563")
          , ("Method", "GET")
          , ("PathInfo", "/Sites-mysite11_us-Site/default/Product-Detail/C1793844563")
          , ("Remote Address", "66.249.69.177")
          ]
        , RequestParams []
        , MalformedRequestParams Nothing
        , StrayHttpHeaders Nothing
        , StackTraceField Nothing
        , JobName Nothing
        , QuotaInfoField Nothing
        , DeprecationInfoField Nothing
        ]
      , [ Timestamp $ LocalTime (fromGregorian 2016 6 8) (TimeOfDay 9 20 16.175)
        , Client dummyClient
        , Filename "file2"
        , LogTypeField SystemLog
        , Level "ERROR"
        , Servlet "JobThread"
        , Site "mysite04_europe"
        , Pipeline "Export-Orders"
        , Caller "Job Runner"
        , SessionID "f5b8943dcd7144e5a316589f00f5b8943dcd"
        , SessionType $ Just "JOB"
        , RequestID $ Just "87d1fea7715d53dc850a402efa"
        , Logger "system.core"
        , Message
          "core.evalScript (org.mozilla.javascript.EcmaError: TypeError: Cannot call method \"addMessage\" of null \
          \([Pipeline:Export:${...=(CurrentWorkflowComponentInstance.addMessage(\"Starting to export \
          \\"+OrdersToExportCount+\" orders.\",'INFO'))}]#1))"
        , SystemInfo
          [ ("RequestID", "87d1fea7715d53dc850a402efa")
          , ("SessionType", "JOB")
          , ("SessionID", "f5b8943dcd7144e5a316589f00f5b8943dcd")
          , ("ServerName", "")
          , ("ServerPort", "-1")
          ]
        , RequestInfo
          [ ("URI", "")
          , ("Method", "")
          , ("PathInfo", "")
          , ("QueryString", "")
          , ("Remote User", "")
          ]
        , RequestParams []
        , MalformedRequestParams Nothing
        , StrayHttpHeaders Nothing
        , StackTraceField Nothing
        , JobName $ Just "Export - Orders"
        , QuotaInfoField Nothing
        , DeprecationInfoField Nothing
        ]
      , [ Timestamp $ LocalTime (fromGregorian 2016 6 6) (TimeOfDay 0 0 54.096)
        , Client dummyClient
        , Filename "file2"
        , LogTypeField SystemLog
        , Level "ERROR"
        , Servlet ""
        , Site ""
        , Pipeline ""
        , Caller "OnChangeIndexer-thread-1"
        , SessionID ""
        , SessionType Nothing
        , RequestID Nothing
        , Logger "com.demandware.component.search3.index.SearchSvcRequestFactory"
        , Message
          "2172177384246719488  Outdated order found. Loaded OCA 297560769, Index Request OCA 297560770 for \
          \order (W1028846963, bdXcYiaagY3YYaaadcEFVsFRb8)"
        , SystemInfo []
        , RequestInfo []
        , RequestParams []
        , MalformedRequestParams Nothing
        , StrayHttpHeaders Nothing
        , StackTraceField Nothing
        , JobName Nothing
        , QuotaInfoField Nothing
        , DeprecationInfoField Nothing
        ]
      , [ Timestamp $ LocalTime (fromGregorian 2016 7 12) (TimeOfDay 5 5 51.710)
        , Client dummyClient
        , Filename "file2"
        , LogTypeField SystemLog
        , Level "ERROR"
        , Servlet "PipelineCallServlet"
        , Site "dw-business-manager"
        , Pipeline "ViewOrder_52-Dispatch"
        , Caller "PipelineCall"
        , SessionID "1ZMspDPCWtnbJlw3p4fJq0ou9fImvJOOcRPO_A02AH6rrbc5VtVLkwj6HsH0oP1flOrZJLgS3seEdQNKHaqPqw=="
        , SessionType $ Just "BUSINESSMGR"
        , RequestID $ Just "qVbl9leEeq_IAgAK-0-00"
        , Logger "system.core"
        , Message
          "core.ISML_ISPRINTError (161, java.lang.NullPointerException(null))"
        , SystemInfo
          [ ("RequestID", "qVbl9leEeq_IAgAK-0-00")
          , ("SessionType", "STOREFRONT")
          , ("SessionID", "1ZMspDPCWtnbJlw3p4fJq0ou9fImvJOOcRPO_A02AH6rrbc5VtVLkwj6HsH0oP1flOrZJLgS3seEdQNKHaqPqw==")
          , ("ServerName", "localhost")
          , ("ServerPort", "10161")
          ]
        , RequestInfo
          [ ("URI", "/servlet/Beehive/Sites-Site/default/ViewOrder_52-Dispatch")
          , ("Method", "GET")
          , ("PathInfo", "/Sites-Site/default/ViewOrder_52-Dispatch")
          , ("QueryString", "LineItemGroupUUID=bdy72iaag01XYaaadnHNJA3bIk&OrderID=deEGwiaag0AboaaadnrNJA3cIk&packageDetail=packageDetail")
          , ("Remote Address", "172.127.160.253")
          ]
        , RequestParams
          [ ("LineItemGroupUUID", "bdy72iaag01XYaaadnHNJA3bIk")
          , ("packageDetail", "packageDetail")
          , ("OrderID", "deEGwiaag0AboaaadnrNJA3cIk")
          ]
        , MalformedRequestParams Nothing
        , StrayHttpHeaders Nothing
        , StackTraceField Nothing
        , JobName Nothing
        , QuotaInfoField Nothing
        , DeprecationInfoField Nothing
        ]
      ]

  describe "Quota log entries" $ do
    assertLogValues
      (quotaLogParser $ DwLogMeta dummyClient "file3" "some-instance")
      "test-logs/quota-blade0-2-appserver-20160608.log"
      [ [ Timestamp $ LocalTime (fromGregorian 2016 6 8) (TimeOfDay 0 0 42.327)
        , Client dummyClient
        , Filename "file3"
        , LogTypeField QuotaLog
        , Level "ERROR"
        , Servlet "PipelineCallServlet"
        , Site "mysite11_us"
        , Pipeline "Search-Show"
        , Caller "PipelineCall"
        , SessionID "pIV4fT_ayTxfJYmdhGlhWZ0PpkWEtE0GesXjFYpoyHzXMcIzwMp5J2OLEejoNuzsPTNtZqCgPefBHXlCylj9-w=="
        , SessionType Nothing
        , RequestID Nothing
        , Logger "api.dw.catalog.Category.getOnlineSubCategories()@SF"
        , Message
          "current location: request/site Sites-mysite11_us-Site/top pipeline Search-Show/interaction node/template \
          \www_005fstore.default_.rendering.category.categorylanding_005foptionT_002d242b38020b1d104830e63b7f9a74231faaafed1e\
          \/template www_005fstore.default_.search.components.categoryleftnav_002d4821b06398965180ba3edbfc7e57882f8566bfb1"
        , SystemInfo []
        , RequestInfo []
        , RequestParams []
        , MalformedRequestParams Nothing
        , StrayHttpHeaders Nothing
        , StackTraceField Nothing
        , JobName Nothing
        , QuotaInfoField $ Just $ QuotaInfo
          { warn = Nothing
          , limit = 0
          , exceededTimes = 226
          , maxActual = 1
          , quotaType = "internal"
          }
        , DeprecationInfoField Nothing
        ]
      , [ Timestamp $ LocalTime (fromGregorian 2016 6 8) (TimeOfDay 0 3 27.717)
        , Client dummyClient
        , Filename "file3"
        , LogTypeField QuotaLog
        , Level "ERROR"
        , Servlet "PipelineCallServlet"
        , Site "mysite11_us"
        , Pipeline "COSummary-Submit"
        , Caller "PipelineCall"
        , SessionID "vOolYGeJP5d4oHhSxF41zLDdlxAv0JYwNOBbSYXy4DA5YFMx7RhM2RRjTUJzqzMv_H4TtmOFzyvlWehM54rIBQ=="
        , SessionType Nothing
        , RequestID Nothing
        , Logger "object.OrderPO"
        , Message
          "current location: request/site Sites-mysite11_us-Site/top pipeline COSummary-Submit/pipelet CreateOrder2"
        , SystemInfo []
        , RequestInfo []
        , RequestParams []
        , MalformedRequestParams Nothing
        , StrayHttpHeaders Nothing
        , StackTraceField Nothing
        , JobName Nothing
        , QuotaInfoField $ Just $ QuotaInfo
          { warn = Just 600000
          , limit = 1000000
          , exceededTimes = 4
          , maxActual = 2351091
          , quotaType = "internal"
          }
        , DeprecationInfoField Nothing
        ]
      , [ Timestamp $ LocalTime (fromGregorian 2016 6 8) (TimeOfDay 0 6 21.301)
        , Client dummyClient
        , Filename "file3"
        , LogTypeField QuotaLog
        , Level "WARN"
        , Servlet "PipelineCallServlet"
        , Site "mysite03_us"
        , Pipeline "COSummary-Submit"
        , Caller "PipelineCall"
        , SessionID "k0XC0wtotVQbjReaO4AiOpuVucplYjHUBHr9TVPEM-vfWOQCUgdAtNr-Op4f36Gab3OsJOw8UKuZZp2KzMLiSA=="
        , SessionType Nothing
        , RequestID Nothing
        , Logger "api.dw.net.HTTPClient.send()@SF"
        , Message
          "current location: request/site Sites-mysite03_us-Site/top pipeline COSummary-Submit/pipelet Script/script www_store\
          \:checkout/GetConfirmationRecommendations.ds:12/script www_store:checkout/GetConfirmationRecommendations.ds:34"
        , SystemInfo []
        , RequestInfo []
        , RequestParams []
        , MalformedRequestParams Nothing
        , StrayHttpHeaders Nothing
        , StackTraceField Nothing
        , JobName Nothing
        , QuotaInfoField $ Just $ QuotaInfo
          { warn = Just 5
          , limit = 8
          , exceededTimes = 4
          , maxActual = 7
          , quotaType = "enforced"
          }
        , DeprecationInfoField Nothing
        ]
      , [ Timestamp $ LocalTime (fromGregorian 2016 6 8) (TimeOfDay 0 8 2.506)
        , Client dummyClient
        , Filename "file3"
        , LogTypeField QuotaLog
        , Level "ERROR"
        , Servlet ""
        , Site ""
        , Pipeline ""
        , Caller "MulticastListener"
        , SessionID ""
        , SessionType Nothing
        , RequestID Nothing
        , Logger "object.OrderPO"
        , Message ""
        , SystemInfo []
        , RequestInfo []
        , RequestParams []
        , MalformedRequestParams Nothing
        , StrayHttpHeaders Nothing
        , StackTraceField Nothing
        , JobName Nothing
        , QuotaInfoField $ Just $ QuotaInfo
          { warn = Just 600000
          , limit = 1000000
          , exceededTimes = 7
          , maxActual = 2351198
          , quotaType = "internal"
          }
        , DeprecationInfoField Nothing
        ]
      , [ Timestamp $ LocalTime (fromGregorian 2016 6 5) (TimeOfDay 0 20 1.604)
        , Client dummyClient
        , Filename "file3"
        , LogTypeField QuotaLog
        , Level "ERROR"
        , Servlet "JobThread"
        , Site ""
        , Pipeline "Export-Orders"
        , Caller ""
        , SessionID ""
        , SessionType Nothing
        , RequestID Nothing
        , Logger "api.queryObjects@JOB"
        , Message "current location: job/job Export - Orders/top pipeline Export-Orders"
        , SystemInfo []
        , RequestInfo []
        , RequestParams []
        , MalformedRequestParams Nothing
        , StrayHttpHeaders Nothing
        , StackTraceField Nothing
        , JobName $ Just "Export - Orders"
        , QuotaInfoField $ Just $ QuotaInfo
          { warn = Nothing
          , limit = 0
          , exceededTimes = 100
          , maxActual = 1
          , quotaType = "internal"
          }
        , DeprecationInfoField Nothing
        ]
      ]

  describe "API log entries" $ do
    assertLogValues
      (apiLogParser $ DwLogMeta dummyClient "file4" "some-instance")
      "test-logs/api-blade1-1-appserver-20160616.log"
      [ [ Timestamp $ LocalTime (fromGregorian 2016 7 1) (TimeOfDay 17 24 1.404)
        , Client dummyClient
        , Filename "file4"
        , LogTypeField ApiLog
        , Level "ERROR"
        , Servlet ""
        , Site "mysite12_us"
        , Pipeline "bc_impex/ProcessABTestStatisticsImport"
        , Caller ""
        , SessionID ""
        , SessionType Nothing
        , RequestID Nothing
        , Logger ""
        , Message "top level execution of system pipeline in custom site"
        , SystemInfo []
        , RequestInfo []
        , RequestParams []
        , MalformedRequestParams Nothing
        , StrayHttpHeaders Nothing
        , StackTraceField Nothing
        , JobName Nothing
        , QuotaInfoField Nothing
        , DeprecationInfoField Nothing
        , ApiInfoField $ Just ApiInfo
          { violation = Just "Pipeline usage"
          , method = Nothing
          }
        ]
      , [ Timestamp $ LocalTime (fromGregorian 2016 7 1) (TimeOfDay 17 39 38.132)
        , Client dummyClient
        , Filename "file4"
        , LogTypeField ApiLog
        , Level ""
        , Servlet ""
        , Site ""
        , Pipeline ""
        , Caller ""
        , SessionID ""
        , SessionType Nothing
        , RequestID Nothing
        , Logger ""
        , Message
          "WSDL file: /remote/f_aako/aako/aako_stg/sharedata/cartridges/develop_A/int_svs/cartridge/webreferences\
          \/StoreValue_GiftCardOnline.wsdl, Binding: {http://service.svsxml.svs.com}SVSXMLWaySoapBinding, Style: document"
        , SystemInfo []
        , RequestInfo []
        , RequestParams []
        , MalformedRequestParams Nothing
        , StrayHttpHeaders Nothing
        , StackTraceField Nothing
        , JobName Nothing
        , QuotaInfoField Nothing
        , DeprecationInfoField Nothing
        , ApiInfoField Nothing
        ]
      , [ Timestamp $ LocalTime (fromGregorian 2016 6 16) (TimeOfDay 0 5 24.286)
        , Client dummyClient
        , Filename "file4"
        , LogTypeField ApiLog
        , Level ""
        , Servlet ""
        , Site ""
        , Pipeline ""
        , Caller ""
        , SessionID ""
        , SessionType Nothing
        , RequestID Nothing
        , Logger ""
        , Message
          "API Method dw.util.Map.values() has been changed in a newer API Version. Please consider updating."
        , SystemInfo []
        , RequestInfo []
        , RequestParams []
        , MalformedRequestParams Nothing
        , StrayHttpHeaders Nothing
        , StackTraceField Nothing
        , JobName Nothing
        , QuotaInfoField Nothing
        , DeprecationInfoField Nothing
        , ApiInfoField $ Just ApiInfo
          { violation = Nothing
          , method = Just "dw.util.Map.values()"
          }
        ]
      , [ Timestamp $ LocalTime (fromGregorian 2016 6 16) (TimeOfDay 5 51 3.698)
        , Client dummyClient
        , Filename "file4"
        , LogTypeField ApiLog
        , Level "INFO"
        , Servlet ""
        , Site ""
        , Pipeline ""
        , Caller ""
        , SessionID ""
        , SessionType Nothing
        , RequestID Nothing
        , Logger ""
        , Message
          "intended unchecked pipeline dictionary access PIPELET: \
          \com.demandware.component.foundation.pipelet.common.RemoveDictionaryValue"
        , SystemInfo []
        , RequestInfo []
        , RequestParams []
        , MalformedRequestParams Nothing
        , StrayHttpHeaders Nothing
        , StackTraceField Nothing
        , JobName Nothing
        , QuotaInfoField Nothing
        , DeprecationInfoField Nothing
        , ApiInfoField $ Just ApiInfo
          { violation = Just "PipelineDictionary usage"
          , method = Nothing
          }
        ]
      , [ Timestamp $ LocalTime (fromGregorian 2016 6 16) (TimeOfDay 5 51 3.701)
        , Client dummyClient
        , Filename "file4"
        , LogTypeField ApiLog
        , Level "ERROR"
        , Servlet ""
        , Site ""
        , Pipeline ""
        , Caller ""
        , SessionID ""
        , SessionType Nothing
        , RequestID Nothing
        , Logger ""
        , Message
          "unknown property value class PIPELET: \
          \com.demandware.site.bm.pipelet.redirect.URLRedirects KEY: Site"
        , SystemInfo []
        , RequestInfo []
        , RequestParams []
        , MalformedRequestParams Nothing
        , StrayHttpHeaders Nothing
        , StackTraceField Nothing
        , JobName Nothing
        , QuotaInfoField Nothing
        , DeprecationInfoField Nothing
        , ApiInfoField $ Just ApiInfo
          { violation = Just "PipelineDictionary usage"
          , method = Nothing
          }
        ]
      , [ Timestamp $ LocalTime (fromGregorian 2016 6 16) (TimeOfDay 0 38 52.808)
        , Client dummyClient
        , Filename "file4"
        , LogTypeField ApiLog
        , Level ""
        , Servlet ""
        , Site ""
        , Pipeline ""
        , Caller ""
        , SessionID ""
        , SessionType Nothing
        , RequestID Nothing
        , Logger ""
        , Message
          "TEMPLATE:www_store/default/account/payment/paymentinstrumentlist KEY: bctext2"
        , SystemInfo []
        , RequestInfo []
        , RequestParams []
        , MalformedRequestParams Nothing
        , StrayHttpHeaders Nothing
        , StackTraceField Nothing
        , JobName Nothing
        , QuotaInfoField Nothing
        , DeprecationInfoField Nothing
        , ApiInfoField $ Just ApiInfo
          { violation = Just "PipelineDictionary usage"
          , method = Nothing
          }
        ]
      , [ Timestamp $ LocalTime (fromGregorian 2016 6 16) (TimeOfDay 9 54 5.492)
        , Client dummyClient
        , Filename "file4"
        , LogTypeField ApiLog
        , Level ""
        , Servlet ""
        , Site ""
        , Pipeline ""
        , Caller ""
        , SessionID ""
        , SessionType Nothing
        , RequestID Nothing
        , Logger ""
        , Message
          "Read from ResettableIterator buffer in Stores-Find"
        , SystemInfo []
        , RequestInfo []
        , RequestParams []
        , MalformedRequestParams Nothing
        , StrayHttpHeaders Nothing
        , StackTraceField Nothing
        , JobName Nothing
        , QuotaInfoField Nothing
        , DeprecationInfoField Nothing
        , ApiInfoField Nothing
        ]
      , [ Timestamp $ LocalTime (fromGregorian 2016 6 16) (TimeOfDay 12 18 37.976)
        , Client dummyClient
        , Filename "file4"
        , LogTypeField ApiLog
        , Level ""
        , Servlet ""
        , Site ""
        , Pipeline ""
        , Caller ""
        , SessionID ""
        , SessionType Nothing
        , RequestID Nothing
        , Logger ""
        , Message
          "'com.demandware.beehive.foundation.util.URL' placed into session."
        , SystemInfo []
        , RequestInfo []
        , RequestParams []
        , MalformedRequestParams Nothing
        , StrayHttpHeaders Nothing
        , StackTraceField Nothing
        , JobName Nothing
        , QuotaInfoField Nothing
        , DeprecationInfoField Nothing
        , ApiInfoField $ Just ApiInfo
          { violation = Just "SessionDictionary usage"
          , method = Nothing
          }
        ]
      ]

  describe "API Deprecation log entries" $ do
    assertLogValues
      (apiDeprecationLogParser $ DwLogMeta dummyClient "file5" "some-instance")
      "test-logs/api-deprecation-blade6-7-appserver-20160705.log"
      [ [ Timestamp $ LocalTime (fromGregorian 2016 7 5) (TimeOfDay 0 0 0.041)
        , Client dummyClient
        , Filename "file5"
        , LogTypeField ApiDeprecationLog
        , Level ""
        , Servlet ""
        , Site "mysite08_us"
        , Pipeline "__Analytics-Tracking"
        , Caller ""
        , SessionID ""
        , SessionType Nothing
        , RequestID Nothing
        , Logger ""
        , Message ""
        , SystemInfo []
        , RequestInfo []
        , RequestParams []
        , MalformedRequestParams Nothing
        , StrayHttpHeaders Nothing
        , StackTraceField Nothing
        , JobName Nothing
        , QuotaInfoField Nothing
        , DeprecationInfoField $ Just $ DeprecationInfo
          { scriptMethod = Just "dw.system.Site@getCurrencyCode()"
          , decisionNode = Nothing
          , depWhere = "pipelet Script/script www_store:util/CheckLocaleCurrency.ds:45"
          , times = 1
          }
        ]
      , [ Timestamp $ LocalTime (fromGregorian 2016 7 5) (TimeOfDay 0 7 2.059)
        , Client dummyClient
        , Filename "file5"
        , LogTypeField ApiDeprecationLog
        , Level ""
        , Servlet ""
        , Site "mysite07_us"
        , Pipeline "Cart-AddCoupon"
        , Caller ""
        , SessionID ""
        , SessionType Nothing
        , RequestID Nothing
        , Logger ""
        , Message ""
        , SystemInfo []
        , RequestInfo []
        , RequestParams []
        , MalformedRequestParams Nothing
        , StrayHttpHeaders Nothing
        , StackTraceField Nothing
        , JobName Nothing
        , QuotaInfoField Nothing
        , DeprecationInfoField $ Just $ DeprecationInfo
          { scriptMethod = Nothing
          , decisionNode = Just "Cart"
          , depWhere = "decision node"
          , times = 4
          }
        ]
      , [ Timestamp $ LocalTime (fromGregorian 2016 7 5) (TimeOfDay 13 20 56.701)
        , Client dummyClient
        , Filename "file5"
        , LogTypeField ApiDeprecationLog
        , Level ""
        , Servlet ""
        , Site "mysite07_eu"
        , Pipeline "GlobalCollect-HML_RTB_Return"
        , Caller ""
        , SessionID ""
        , SessionType Nothing
        , RequestID Nothing
        , Logger ""
        , Message
          "Type 'dw.order.Order' not allowed as attribute value for a session."
        , SystemInfo []
        , RequestInfo []
        , RequestParams []
        , MalformedRequestParams Nothing
        , StrayHttpHeaders Nothing
        , StackTraceField Nothing
        , JobName Nothing
        , QuotaInfoField Nothing
        , DeprecationInfoField $ Just $ DeprecationInfo
          { scriptMethod = Nothing
          , decisionNode = Nothing
          , depWhere = "pipelet Assign"
          , times = 2
          }
        ]
      , [ Timestamp $ LocalTime (fromGregorian 2016 7 12) (TimeOfDay 8 15 4.756)
        , Client dummyClient
        , Filename "file5"
        , LogTypeField ApiDeprecationLog
        , Level ""
        , Servlet ""
        , Site ""
        , Pipeline "ResponsysFeeds-Product"
        , Caller ""
        , SessionID ""
        , SessionType Nothing
        , RequestID Nothing
        , Logger ""
        , Message ""
        , SystemInfo []
        , RequestInfo []
        , RequestParams []
        , MalformedRequestParams Nothing
        , StrayHttpHeaders Nothing
        , StackTraceField Nothing
        , JobName $ Just "Responsys - Product Feed"
        , QuotaInfoField Nothing
        , DeprecationInfoField $ Just $ DeprecationInfo
          { scriptMethod = Just "dw.catalog.Product@isAvailable()"
          , decisionNode = Nothing
          , depWhere =
              "pipelet Script/script www_jobs:responsys/responsysExportProducts.ds:67/script \
              \www_jobs:responsys/responsysExportProducts.ds:121/script www_jobs:responsys/responsysExportProducts.ds:163"
          , times = 11457
          }
        ]
      ]

--------------------------------------------------------------------------------
-- Test helpers
--------------------------------------------------------------------------------
assertLogValues parser inputFile expected = do
  describe ("from test file " ++ inputFile) $ do
    results <- runIO $ parseFromFile parser [] inputFile
    case sequence results of
      Left err ->
        it "should successfully parse" $ do
          expectationFailure $ show err
      Right logs -> do
        it "should have the expected numbers of logs" $ do
          length logs `shouldBe` length expected
        forM_ (zip3 [1..] expected logs) $ \(logNum, exp, act) -> do
          forM_ exp $ \x ->
            it ("log #" ++ (show logNum) ++ " should have the expected value for " ++ (T.unpack $ fieldTestName x)) $ do
              fieldTestAssert x act

data DwLogFieldTest
  = Client Text
  | Timestamp LocalTime
  | Filename Text
  | LogTypeField LogType
  | Level Text
  | Servlet Text
  | Site Text
  | Pipeline Text
  | Caller Text
  | SessionID Text
  | SessionType (Maybe Text)
  | RequestID (Maybe Text)
  | Logger Text
  | Message Text
  | SystemInfo [(Text, Text)]
  | RequestInfo [(Text, Text)]
  | RequestParams [(Text, Text)]
  | MalformedRequestParams (Maybe Text)
  | StrayHttpHeaders (Maybe Text)
  | StackTraceField (Maybe StackTrace)
  | JobName (Maybe Text)
  | QuotaInfoField (Maybe QuotaInfo)
  | DeprecationInfoField (Maybe DeprecationInfo)
  | ApiInfoField (Maybe ApiInfo)
  deriving (Show, Eq)

fieldTestAssert exp act =
  let f `is` x = f act `shouldBe` x
  in
    case exp of
      Client x                 -> (client . logmeta) `is` x
      Timestamp x              -> timestamp `is` x
      Filename x               -> (filename . logmeta) `is` x
      LogTypeField x           -> logType `is` x
      Level x                  -> level `is` x
      Servlet x                -> servlet `is` x
      Site x                   -> site `is` x
      Pipeline x               -> pipeline `is` x
      Caller x                 -> caller `is` x
      SessionID x              -> sessionID `is` x
      SessionType x            -> sessionType `is` x
      RequestID x              -> requestID `is` x
      Logger x                 -> logger `is` x
      Message x                -> message `is` x
      SystemInfo x             -> systemInfo `is` x
      RequestInfo x            -> requestInfo `is` x
      RequestParams x          -> requestParams `is` x
      MalformedRequestParams x -> malformedRequestParams `is` x
      StrayHttpHeaders x       -> strayHttpHeaders `is` x
      StackTraceField x        -> stackTrace `is` x
      JobName x                -> jobName `is` x
      QuotaInfoField x         -> quotaInfo `is` x
      DeprecationInfoField x   -> deprecation `is` x
      ApiInfoField x           -> apiInfo `is` x

fieldTestName :: DwLogFieldTest -> Text
fieldTestName x =
  case x of
    Client _                 -> "client"
    Timestamp _              -> "timestamp"
    Filename _               -> "filename"
    LogTypeField _           -> "logType"
    Level _                  -> "level"
    Servlet _                -> "servlet"
    Site _                   -> "site"
    Pipeline _               -> "pipeline"
    Caller _                 -> "caller"
    SessionID _              -> "sessionID"
    SessionType _            -> "sessionType"
    RequestID _              -> "requestID"
    Logger _                 -> "logger"
    Message _                -> "message"
    SystemInfo _             -> "systemInfo"
    RequestInfo _            -> "requestInfo"
    RequestParams _          -> "requestParams"
    MalformedRequestParams _ -> "malformedRequestParams"
    StrayHttpHeaders _       -> "strayHttpHeaders"
    StackTraceField _        -> "stackTrace"
    JobName _                -> "jobName"
    QuotaInfoField _         -> "quotaInfo"
    DeprecationInfoField _   -> "deprecationInfo"
    ApiInfoField _           -> "apiInfo"

dummyClient = "client"
