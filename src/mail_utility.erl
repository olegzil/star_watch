-module(mail_utility).
-export([send_email/3]).
-include("include/macro_definitions.hrl").

send_email(ClientID, ToEmail, ToName) ->
    % Set the necessary variables
    {ApiKeyPublic, ApiKeyPrivate} = server_config_processor:get_email_keys(?SERVER_CONFIG_FILE),
    Url = "https://api.mailjet.com/v3.1/send",
    ContentType = "application/json",
    SandboxMode = false,
    FromEmail = <<"TheTinkerersShop@gmail.com">>,

    PREAMBLE = << "<a href=\"">>,
    POSTAMBLE = << "\">click to complete login</a>" >>,
    LOGIN_CALLBACK_PATH = <<"youtube/login">>,
    LoginAction = <<"?action=complete_login&login_token=">>,
    ClientIDSignature = <<"&client_id=", ClientID/binary>>,
    KeySignature = <<"&key=", ?CLIENT_ACCESS_KEY/binary>>,
    Token = list_to_binary(utils:to_string(utils:v4())),
    MainLink = << ?LOGIN_CALLBACK_ADDRESS_ACTIVE/binary, LOGIN_CALLBACK_PATH/binary, LoginAction/binary, Token/binary, ClientIDSignature/binary, KeySignature/binary >>,
    Link = <<PREAMBLE/binary, MainLink/binary, POSTAMBLE/binary >>,
    % Create the request payload
    Payload = #{<<"SandboxMode">> => SandboxMode,
                <<"Messages">> => [
                    #{<<"From">> => #{<<"Email">> => FromEmail, <<"Name">> => ?LOGIN_EMAIL_FROM_NAME},
                      <<"To">> => [#{<<"Email">> => ToEmail, <<"Name">> => ToName}],
                      <<"Subject">> => ?LOGIN_EMAIL_SUBJECT,
                      <<"HTMLPart">> => Link
                    }
                ]
              },

    % Convert the payload to JSON
    JsonPayload = binary_to_list(jiffy:encode(Payload)),

    % Construct the HTTP request
    Options = [{ssl, [{verify, verify_none}]}],
    Headers = [{"Content-Type", ContentType},
               {"Authorization", "Basic " ++ base64:encode_to_string(ApiKeyPublic ++ ":" ++ ApiKeyPrivate)}],
    Request = {Url, Headers, "POST", JsonPayload},

    % Perform the HTTP request
    {ok, {StatusCode, _RespHeaders, RespBody}} = httpc:request(post, Request, Options, [{body_format, binary}]),
    {_, Code, _} = StatusCode,

    % Handle the response
    io:format("Status code: ~p~n", [Code]),
    io:format("Response Body: ~p~n", [RespBody]),
    case Code of
        200 ->
            {ok, Token};
        _ErrorCode ->
            {error, <<"email notification failed">>}
    end.