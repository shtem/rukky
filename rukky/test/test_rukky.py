import re, os, subprocess, pytest

TEST_CORRECT_RESULTS = {
    "addNum": 210,
    "factorial": 120,
    "fibonacci": [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55],
    "lcm": 216,
    "palindrome": "true",
    "prime": [907, 911, 919, 929, 937, 941, 947, 953, 967, 971, 977, 983, 991, 997],
    "rotateArray": [3, 4, 5, 6, 7, 1, 2],
    "unary": 4,
}


def get_test_name(filePath: str):
    path = r"" + filePath
    baseName = os.path.basename(os.path.normpath(path))
    testName, _ = os.path.splitext(baseName)
    return testName


def get_test_result(filePath: str):
    path = r"" + filePath
    command = (
        f"python C:\\Users\\temis\\dev\\projects\\rukky\\rukky\\rukky.py -f {path}"
    )
    result = subprocess.run(command, stdout=subprocess.PIPE)
    testOut = result.stdout.decode("utf-8").replace("\r\n", "").replace("\n", "")
    search = re.search("result:\s+(.+)", testOut)
    _, match = search.group().split(":")
    return match.strip()


def test_sucess():
    pass


def test_failure():
    pass
